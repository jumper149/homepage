set terminal svg size 800,400

set xlabel 'time/ms' offset 0,3.3
set xrange [0:50]
set ylabel 'amplitude' offset 5.2,-2.3
set yrange [-1:1]
set grid
set samples 1000

unset title
unset colorbox

scale=1000
f=40

set output '2ndHarmonic.svg'
set palette defined \
  ( 0 "dark-blue" \
  , 0.5 "blue" \
  , 1 "light-blue" \
  )
plot 0.8*sin(scale*x*2*pi*1*f)+0.3*sin(scale*x*2*pi*2*f) with lines linewidth 2 dt 1 linecolor palette frac 0.5 title 'sum of 1st and 2nd harmonic' \
   , 0.8*sin(scale*x*2*pi*1*f)                           with lines linewidth 2 dt 4 linecolor palette frac 0   title '1st harmonic' \
   ,                           0.3*sin(scale*x*2*pi*2*f) with lines linewidth 2 dt 5 linecolor palette frac 1   title '2nd harmonic' \


set output '3rdHarmonic.svg'
set palette defined \
  ( 0 "dark-green" \
  , 0.5 "green" \
  , 1 "light-green" \
  )
plot 0.8*sin(scale*x*2*pi*1*f)+0.3*sin(scale*x*2*pi*3*f) with lines linewidth 2 dt 1 linecolor palette frac 0.5 title 'sum of 1st and 3rd harmonic' \
   , 0.8*sin(scale*x*2*pi*1*f)                           with lines linewidth 2 dt 4 linecolor palette frac 0   title '1st harmonic' \
   ,                           0.3*sin(scale*x*2*pi*3*f) with lines linewidth 2 dt 5 linecolor palette frac 1   title '3rd harmonic' \
