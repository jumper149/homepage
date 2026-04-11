#!/usr/bin/env node

const fs = require("fs")
const postcss = require("postcss")
const valueParser = require("postcss-value-parser")

const file = process.argv[2]
const css = fs.readFileSync(file, "utf8")

const colorMap = new Map()
let counter = 1

// --- detect ANY valid CSS color token ---
function isColorNode(node) {
  if (node.type !== "function") return false

  const name = node.value.toLowerCase()

  return (
    name === "rgb" ||
    name === "rgba" ||
    name === "hsl" ||
    name === "hsla"
  )
}

// hex detection (separate because it's a word token)
function isHex(v) {
  return /^#([0-9a-f]{3}|[0-9a-f]{6})$/i.test(v)
}

function getVar(color) {
  if (!colorMap.has(color)) {
    colorMap.set(color, `--c${counter++}`)
  }
  return colorMap.get(color)
}

// parse CSS safely
const root = postcss.parse(css)

root.walkDecls(decl => {
  const parsed = valueParser(decl.value)

  let changed = false

  parsed.walk(node => {
    // --- HEX COLORS ---
    if (node.type === "word" && isHex(node.value)) {
      const color = node.value
      const v = getVar(color)
      node.value = `var(${v})`
      changed = true
    }

    // --- rgb()/hsl() FUNCTIONS ---
    if (node.type === "function" && isColorNode(node)) {
      const original = valueParser.stringify(node)
      const v = getVar(original)

      // replace whole function
      node.type = "word"
      node.value = `var(${v})`
      delete node.nodes

      changed = true
    }
  })

  if (changed) {
    decl.value = parsed.toString()
  }
})

// --- generate variables ---
function buildRoot() {
  let out = ":root {\n"
  for (const [color, v] of colorMap.entries()) {
    out += `  ${v}: ${color};\n`
  }
  out += "}\n"
  return out
}

// --- dark mode (no transformation, just duplication) ---
function buildDark() {
  let out = "@media (prefers-color-scheme: dark) {\n:root {\n"
  for (const [color, v] of colorMap.entries()) {
    out += `  ${v}: ${color};\n`
  }
  out += "}\n}\n"
  return out
}

// output
const output =
  buildRoot() +
  buildDark() +
  "\n" +
  root.toString()

fs.writeFileSync("out.css", output)

console.log(`✔ extracted ${colorMap.size} colors`)
console.log("✔ supports hex + rgb() + hsl() modern syntax")
console.log("✔ wrote out.css")

