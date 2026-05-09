set terminal svg size 800,400

set xlabel 'time/ms' offset 0,3.4
set xrange [0:100]
set ylabel 'amplitude' offset 6.8,-2.3
set yrange [-4:4]
set grid
set key bottom right
set samples 1000

unset title
unset colorbox

f=1000/40
A=3
B=3
C=B-0.3
clipA(y) = (y > A) ? A : y
clipB(y) = 0.3 + ((y > B-0.3) ? B-0.3 : ((y < -0.3) ? -0.3 : y))
clipC(y) = ((y > C) ? C : ((y < -C) ? -C : y))

set arrow from graph 0, first 0 to graph 1, first 0 nohead lw 1 lc rgb "gray"

set output 'classA.svg'
plot clipA(1.8+1.5*sin(x*2*pi*1/f)) with lines linewidth 2 linecolor rgb "blue" dt 1 title 'clipping 40Hz signal' \
   , 1.8 with lines linewidth 2 dt 2 linecolor rgb "salmon" title 'old zero crossing of the signal' \


set output 'classAB.svg'
plot clipB(3*sin(x*2*pi/f)) with lines linewidth 2 linecolor rgb "dark-green" dt 1 title 'positive half of a clipping 40Hz signal' \
   , clipB(3*sin((x+12.5)*2*pi/f)) with lines linewidth 2 linecolor rgb "green" dt 1 title 'negative half of a clipping 40Hz signal' \
   , 0.3 with lines linewidth 2 dt 2 linecolor rgb "salmon" title 'old zero crossing of the signal' \


set xrange [0:50]
set yrange [-2:2]
set ylabel 'amplitude' offset 8.7,-2.3

set key top right

set output 'classAvsAB.svg'
plot clipA(1.8+1.5*sin(x*2*pi*1/f))-1.724 with lines linewidth 2 dt 1 linecolor rgb "blue" title 'clipped 40Hz signal after class A amplifier' \
   , 0.5*clipC(3*sin(x*2*pi*1/f))    with lines linewidth 2 dt 1 linecolor rgb "green" title 'clipped 40Hz signal after class AB amplifier' \

