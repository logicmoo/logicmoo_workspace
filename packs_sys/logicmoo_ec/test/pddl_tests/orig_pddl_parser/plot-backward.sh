#!/bin/sh

find ./output/ -maxdepth 1 -empty -exec rm {} \;
./average.sh

 
for i in blocks elevators freecell gripper hanoi ; do #

gnuplot << EOF
set terminal postscript eps rounded color size 5,2
#set output "charts/$i-backward-time.eps"
set output "| tee charts/$i-backward-time.eps | ps2pdf -dEPSCrop - charts/$i-backward-time.pdf" 

set xtics rotate by -45
set ylabel "Time [Seconds]"
set logscale y 
set grid y

set key out top Left reverse
                                                      
plot  "output/$i-backward-dfs-h_0.out.avg"								using 2:xtic(1) title 'DFS h_0' with linespoints lw 2, \
      "output/$i-backward-dfs-h_diff.out.avg"						using 2:xtic(1) title 'DFS h_{diff}' with linespoints lw 2, \
      "output/$i-backward-dfs-h_add.out.avg"							using 2:xtic(1) title 'DFS h_{add}' with linespoints lw 2, \
      "output/$i-backward-dfs-h_max.out.avg"							using 2:xtic(1) title 'DFS h_{max}' with linespoints lw 2, \
      "output/$i-backward-bfs.out.avg"									  using 2:xtic(1) title 'BFS' with linespoints lw 2, \
      "output/$i-backward-iddfs.out.avg"									using 2:xtic(1) title 'IDDFS' with linespoints lw 2, \
      "output/$i-backward-a-star-h_0.out.avg"						using 2:xtic(1) title 'A* h_0' with linespoints lw 2, \
      "output/$i-backward-a-star-h_diff.out.avg"					using 2:xtic(1) title 'A* h_{diff}' with linespoints lw 2, \
      "output/$i-backward-a-star-h_add.out.avg"					using 2:xtic(1) title 'A* h_{add}' with linespoints lw 2, \
      "output/$i-backward-a-star-h_max.out.avg"					using 2:xtic(1) title 'A* h_{max}' with linespoints lw 2, \
      "output/$i-backward-wa-star-h_0.out.avg"						using 2:xtic(1) title 'WA* h_0' with linespoints lw 2, \
      "output/$i-backward-wa-star-h_diff.out.avg"				using 2:xtic(1) title 'WA* h_{diff}' with linespoints lw 2, \
      "output/$i-backward-wa-star-h_add.out.avg"					using 2:xtic(1) title 'WA* h_{add}' with linespoints lw 2, \
      "output/$i-backward-wa-star-h_max.out.avg"					using 2:xtic(1) title 'WA* h_{max}' with linespoints lw 2
EOF


gnuplot << EOF
set terminal postscript eps rounded color size 5,7
set output "| tee charts/$i-backward.eps | ps2pdf -dEPSCrop - charts/$i-backward.pdf"

set multiplot layout 4,1 scale 1,1
set format x ""
unset xtics
set ylabel "Time [Seconds]"
set logscale y 
set grid y
set lmargin 12
set rmargin 20

#set key inside left top vertical Left reverse enhanced autotitles columnhead nobox
set key out top Left reverse
                                                      
plot  "output/$i-backward-dfs-h_0.out.avg"								using 2 title 'DFS h_0' with linespoints lw 2, \
      "output/$i-backward-dfs-h_diff.out.avg"						using 2 title 'DFS h_{diff}' with linespoints lw 2, \
      "output/$i-backward-dfs-h_add.out.avg"							using 2 title 'DFS h_{add}' with linespoints lw 2, \
      "output/$i-backward-dfs-h_max.out.avg"							using 2 title 'DFS h_{max}' with linespoints lw 2, \
      "output/$i-backward-bfs.out.avg"									  using 2 title 'BFS' with linespoints lw 2, \
      "output/$i-backward-iddfs.out.avg"									using 2 title 'IDDFS' with linespoints lw 2, \
      "output/$i-backward-a-star-h_0.out.avg"						using 2 title 'A* h_0' with linespoints lw 2, \
      "output/$i-backward-a-star-h_diff.out.avg"					using 2 title 'A* h_{diff}' with linespoints lw 2, \
      "output/$i-backward-a-star-h_add.out.avg"					using 2 title 'A* h_{add}' with linespoints lw 2, \
      "output/$i-backward-a-star-h_max.out.avg"					using 2 title 'A* h_{max}' with linespoints lw 2, \
      "output/$i-backward-wa-star-h_0.out.avg"						using 2 title 'WA* h_0' with linespoints lw 2, \
      "output/$i-backward-wa-star-h_diff.out.avg"				using 2 title 'WA* h_{diff}' with linespoints lw 2, \
      "output/$i-backward-wa-star-h_add.out.avg"					using 2 title 'WA* h_{add}' with linespoints lw 2, \
      "output/$i-backward-wa-star-h_max.out.avg"					using 2 title 'WA* h_{max}' with linespoints lw 2


set ylabel "# of visited nodes"
plot  "output/$i-backward-dfs-h_0.out.avg"								using 3:xtic(1) title 'DFS h_0' with linespoints lw 2, \
      "output/$i-backward-dfs-h_diff.out.avg"						using 3:xtic(1) title 'DFS h_{diff}' with linespoints lw 2, \
      "output/$i-backward-dfs-h_add.out.avg"							using 3:xtic(1) title 'DFS h_{add}' with linespoints lw 2, \
      "output/$i-backward-dfs-h_max.out.avg"							using 3:xtic(1) title 'DFS h_{max}' with linespoints lw 2, \
      "output/$i-backward-bfs.out.avg"									  using 3:xtic(1) title 'BFS' with linespoints lw 2, \
      "output/$i-backward-iddfs.out.avg"									using 3:xtic(1) title 'IDDFS' with linespoints lw 2, \
      "output/$i-backward-a-star-h_0.out.avg"						using 3:xtic(1) title 'A* h_0' with linespoints lw 2, \
      "output/$i-backward-a-star-h_diff.out.avg"					using 3:xtic(1) title 'A* h_{diff}' with linespoints lw 2, \
      "output/$i-backward-a-star-h_add.out.avg"					using 3:xtic(1) title 'A* h_{add}' with linespoints lw 2, \
      "output/$i-backward-a-star-h_max.out.avg"					using 3:xtic(1) title 'A* h_{max}' with linespoints lw 2, \
      "output/$i-backward-wa-star-h_0.out.avg"						using 3:xtic(1) title 'WA* h_0' with linespoints lw 2, \
      "output/$i-backward-wa-star-h_diff.out.avg"				using 3:xtic(1) title 'WA* h_{diff}' with linespoints lw 2, \
      "output/$i-backward-wa-star-h_add.out.avg"					using 3:xtic(1) title 'WA* h_{add}' with linespoints lw 2, \
      "output/$i-backward-wa-star-h_max.out.avg"					using 3:xtic(1) title 'WA* h_{max}' with linespoints lw 2

unset logscale y
set ylabel "Length of solution"

#set style fill solid 1.00 border -1
#set style rectangle back fc lt -3 fillstyle  solid 1.00 border -1
#set style histogram clustered gap 1 

plot  "output/$i-backward-bfs.out.avg"									  using 5:xtic(1) title 'BFS' with linespoints lw 2, \
      "output/$i-backward-iddfs.out.avg"									using 5:xtic(1) title 'IDDFS' with linespoints lw 2, \
      "output/$i-backward-a-star-h_diff.out.avg"					using 5:xtic(1) title 'A* h_{diff}' with linespoints lw 2, \
      "output/$i-backward-a-star-h_add.out.avg"					using 5:xtic(1) title 'A* h_{add}' with linespoints lw 2, \
      "output/$i-backward-wa-star-h_0.out.avg"						using 5:xtic(1) title 'WA* h_0' with linespoints lw 2, \
      "output/$i-backward-wa-star-h_diff.out.avg"					using 5:xtic(1) title 'WA* h_{diff}' with linespoints lw 2, \
      "output/$i-backward-wa-star-h_add.out.avg"					using 5:xtic(1) title 'WA* h_{add}' with linespoints lw 2, \
      "output/$i-backward-wa-star-h_max.out.avg"					using 5:xtic(1) title 'WA* h_{max}' with linespoints lw 2

unset logscale y
#set origin 0.4,0.1
set ylabel "Memory [MB]"
set xtics rotate by -45

plot  "output/$i-backward-dfs-h_add.out.avg"							using (\$4/1024/1024):xtic(1) title 'DFS' with linespoints lw 2, \
      "output/$i-backward-bfs.out.avg"									  using (\$4/1024/1024):xtic(1) title 'BFS' with linespoints lw 2, \
      "output/$i-backward-iddfs.out.avg"									using (\$4/1024/1024):xtic(1) title 'IDDFS' with linespoints lw 2, \
      "output/$i-backward-a-star-h_0.out.avg"						using (\$4/1024/1024):xtic(1) title 'A* h_0' with linespoints lw 2, \
      "output/$i-backward-a-star-h_add.out.avg"					using (\$4/1024/1024):xtic(1) title 'A*' with linespoints lw 2, \
      "output/$i-backward-wa-star-h_0.out.avg"						using (\$4/1024/1024):xtic(1) title 'WA* h_0' with linespoints lw 2, \
      "output/$i-backward-wa-star-h_add.out.avg"					using (\$4/1024/1024):xtic(1) title 'WA*' with linespoints lw 2
      
set nomultiplot 



#set terminal png transparent nocrop enhanced font "/usr/share/fonts/truetype/arial.ttf" 8 size 500,500 
#set output "charts/$i-backward.png" 
#replot

 
 
EOF

done

: <<'END'


ps2pdf 

