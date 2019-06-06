#!/bin/bash

#gnuplot -e "namefilenya='band.spin1n2.m${1}t${1}.svg';batas=$1;judul='$2'" /home/aku/kanazawa/work/calcMic/templates/bandplot.isp1n2.edited.glt

#inkscape -z -e band.spin1n2.m${1}t${1}.jpg -w 420 -h 610 band.spin1n2.m${1}t${1}.svg

echo ============================================================================================================
echo run it using:
echo plotpband.sh judul -yr          folder:spin:judul:orbital@atom-orbital@atom
echo plotpband.sh Judul -bawah:atas  folder:1/2:judul:atom-orbital folder:1/2:judul folder:1/2:judul
echo plotpbanddos.sh resPBANDDOS/9.2.11G20.s@9.invB.Minority 1 '"" -3.5:3.5 dxy@9 nico2o4.invB.11G20:2:"11G20 Minority"'
echo ============================================================================================================


if [ -z "$4" ] ; then exit ;fi

TEMPDIR=$(mktemp -d -p ./)
mkdir -p $TEMPDIR

fOut=$1
useOldBw=$2
judulUtama=$3
yr=$4 # yrange , dimasukkan sebagai "bawah:atas" mis "-10:6"
atomOs=$5
shift;shift; shift;shift;shift
daftarLengkap=$@


ender='
unset multiplot

system "cd '${TEMPDIR}'&& epstopdf hasil.eps && pdftocairo -r 150 -singlefile -jpeg hasil.pdf tmp && convert tmp.jpg -rotate 90 hasil.jpg && rm -f tmp.jpg"
'

isi="
set datafile missing '-'
plot \
0 lt -1 lc rgb 'black' title '' "
total=$(($#))
urutan=1
daftarFolder=""
echo xxxxxxxxxxxxxxxxxxx $@
gapnya="0"
while [ -n "$1" ]; do
  echo check
  perintah="$1"
  IFS=':' read -r -a gambar <<< "$perintah"
  foldernya=${gambar[0]}
  spinnya=${gambar[1]}
  legend=${gambar[2]}
  batasAtasX=$(tail $(ls $foldernya/bnd0*spin$spinnya|tail -1)| sed '/^ $/d' |tail -1|awk '{print $2}')
  daftarFolder="$daftarFolder:$legend@$spinnya@$foldernya"
  echo $perintah
  echo $foldernya
  echo $spinnya
  echo $judulnya
#  tebal="lw 1 dt 2"
#  if [ "$urutan" == "1" ]; then
#    tebal="lw 2 "
#  fi
#  if [ "$#" == "1" ] && [ "$total" != "1" ]; then
#    tebal="lw 2 lc rgb 'black'"
#  fi
for i in $(ls $foldernya/bnd0*spin$spinnya);do
    subjudul="title ''";
#    if [ "$i" == "1" ]; then
#      subjudul="title '$legend'"
#    fi
    plotplate="$plotplate , '$i' u (\$2>6.08991818?0.4+\$2:\$2):(\$2>6.08991818&&\$2<6.09991818?1/0:\$3) with line lc rgb 'black' $subjudul "
  done
  valBand="$(cat $foldernya/bnd*spin$spinnya|sed -e '/^#/d' |awk '{if ($3>0.1) print $2,$3}'|sort -k 2n|head -1)"
  valBandX="$(echo $valBand | awk '{printf("%.1f",$1)}')"
  valBandY="$(echo $valBand | awk '{print $2}')"
  condBand="$(cat $foldernya/bnd*spin$spinnya| sed -e '/^#/d' |awk '{if ($3<=0) print $2,$3}'|sort -k 2nr -u|sed -e '/^ *$/d'|head -1)"
  condBandX="$(echo $condBand | awk '{printf("%.1f",$1)}')"
  condBandY="$(echo $condBand | awk '{print $2}')"
  echo ===c=== $condBand ===================== $condBandX xxxxxxxx $condBandY
  echo ===v=== $valBand ===================== $valBandX xxxxxxxx $valBandY
  bandgap=$(echo $condBand $valBand | awk '{print $4-$2}')
  gapCoord=$(cat $foldernya/bnd*spin$spinnya|runGAPband.hs )
  bandgap=$(echo $gapCoord |  awk '{print $3}')
  echo ===b======================== $bandgap
  if [ "$bandgap" != "0" ]; then
  gapnya="$gapnya:$bandgap@$valBandY"
  arrow="$arrow
  set label sprintf ('{/Symbol D}=%.2feV',$bandgap) at ($valBandX-0.55),($valBandY-0.4) font ',12'
  set arrow from $valBandX,$condBandY to $valBandX,$valBandY heads noborder lw 2 lc rgb 'blue'"
  fi

  urutan=$(($urutan+1))
  shift
done
perintahDOS="/home/aku/kanazawa/dev/cpvoh/app/genPDOSvert.hs $atomOs $gapnya $spinnya $daftarFolder"
echo ===========$perintahDOS
generatedDOS=$($perintahDOS)
echo =======generatedDOS: $generatedDOS
echo ===========DONE:genPDOSvert.hs

#  generatedPBAND=$(genPBAND.hs $foldernya $spinnya 0 $atomnya@$orbitalnya )
  perintahPBAND="/home/aku/kanazawa/dev/cpvoh/app/genPBAND.hs $useOldBw $spinnya 0 $atomOs $daftarFolder"
  echo =======$perintahPBAND
  generatedPBAND=$(/home/aku/kanazawa/dev/cpvoh/app/genPBAND.hs $useOldBw $spinnya 0 $atomOs $daftarFolder)
  echo =======$generatedPBAND
  plotplate="$plotplate, $generatedPBAND"
echo =======DONE:genPBAND.hs

plotplate="$plotplate
unset arrow
$generatedDOS
"

echo "==================bikin TEMPGLT"
TEMPGLT=$(mktemp -p ./)
xrangeatas=$(/home/aku/kanazawa/dev/cpvoh/app/genBandTicks.hs $foldernya | more| head -1 )
ticksbaru=$(/home/aku/kanazawa/dev/cpvoh/app/genBandTicks.hs $foldernya | tail -1 )
echo =======DONE:genBandTicks.hs
cat >> $TEMPGLT << EOF
#!/home/aku/bin/gnuplot -persist
reset
set term post landscape enhanced color "Times-Roman" 12

set output "${TEMPDIR}/hasil.eps"

rydberg=13.605

if (!exists("MP_LEFT"))   MP_LEFT = .1
if (!exists("MP_RIGHT"))  MP_RIGHT = .95
if (!exists("MP_BOTTOM")) MP_BOTTOM = .1
if (!exists("MP_TOP"))    MP_TOP = .9
if (!exists("MP_GAP"))    MP_GAP = 0.05
set multiplot layout 1,2 \
  margins screen MP_LEFT, MP_RIGHT, MP_BOTTOM, MP_TOP spacing screen MP_GAP

set xzeroaxis
set grid
#unset key
set key right top Left
set key samplen 1 spacing 1
set size ratio 1.5
set mytics 10
set ylabel "Energy-E_F (eV)"
# This is given written in subroutine writeband in lm7K/fp/bndfp.F
#set title "Band of Normal Spinel NiCo2O4 A=(Exp) U=(Exp) ".$judul
set title "$judulUtama"
set yrange [$yr]
#set xrange [0.0:6.90682496]
set xrange [0.0:$xrangeatas ]
set grid noy
set grid xtics lt 0 lc rgb "black"

#set style line 1 lc rgb '#ffa0f0'
#set style line 2 lc rgb '#4ee07f'
#set style line 3 lc rgb '#cd0050'
#set style line 4 lc rgb '#0093db'
#set style line 5 lc rgb '#e5c442'
#set style line 6 lc rgb '#0059c9'
#set style line 7 lc rgb '#7f3e50'
set style line 1 lt 2 lw 1 lc rgb '#e41a1c'
set style line 2 lt 6 lw 1 lc rgb '#377eb8'
set style line 3 lt 6 lw 1 lc rgb '#4daf4a'
set style line 4 lt 6 lw 1 lc rgb '#984ea3'
set style line 5 lt 6 lw 1 lc rgb '#ff7f00'
# set style line 1 lt 1 lw 2 lc rgb '#039F74'
# set style line 2 lt 1 lw 2 lc rgb '#981FCB'
# set style line 3 lt 1 lw 2 lc rgb '#d95f02'
# set style line 4 lt 1 lw 2 lc rgb '#4c05c'
# #set style line 5 lt 1 lw 2 lc rgb '#191919'
#set style line 6 lt 1 lw 2 lc rgb '#05c31'
#set style line 7 lt 1 lw 2 lc rgb '#2bce48'
set style line 8 lt 1 lw 2 lc rgb '#ffcc99'
set style line 9 lt 1 lw 2 lc rgb '#808080'
set style line 10 lt 1 lw 2 lc rgb '#94ffb5'
set style line 11 lt 1 lw 2 lc rgb '#8f7c0'
set style line 12 lt 1 lw 2 lc rgb '#9dcc0'
set style line 13 lt 1 lw 2 lc rgb '#c2088'
set style line 14 lt 1 lw 2 lc rgb '#03380'
set style line 15 lt 1 lw 2 lc rgb '#ffa45'
set style line 16 lt 1 lw 2 lc rgb '#ffa8bb'
set style line 17 lt 1 lw 2 lc rgb '#42660'
set style line 18 lt 1 lw 2 lc rgb '#ff010'
set style line 19 lt 1 lw 2 lc rgb '#5ef1f2'
set style line 20 lt 1 lw 2 lc rgb '#0998f'
set style line 21 lt 1 lw 2 lc rgb '#e0ff66'
set style line 22 lt 1 lw 2 lc rgb '#74aff'
set style line 23 lt 1 lw 2 lc rgb '#9900'
set style line 24 lt 1 lw 2 lc rgb '#ffff80'
set style line 25 lt 1 lw 2 lc rgb '#ffff0'
set style line 26 lt 1 lw 2 lc rgb '#ff505'

set style arrow 1 heads size screen 0.01,90 lw 2 lc rgb 'navy'

set key bottom left Left

set xtics ($ticksbaru )
# 'X' 1.00000000,\
# 'W' 1.50000000,\
# 'K' 1.85355339,\
# 'G' 2.91421356,\
# 'L' 3.78023897,\
# 'U' 4.28023897,\
# 'W' 4.78023897,\
# 'L' 5.48734575,\
# 'K'  6.09971818,\
# 'U'  6.52091818,\
# 'X' 7.20682496)

#  'W'   0.0000000000,\
#  'L'   0.7071067812,\
#  '{/Symbol G}'   1.5731321850,\
#  'X'   2.5731321850,\
#  'W'   3.0731321850,\
#  'K'   3.4266855756,\
#  '{/Symbol G}' 5.40854724)

#set x2tics ('W'   0.0000000000,\
#'L'   0.7071067812,\
#'{/Symbol G}'   1.5731321850,\
#'X'   2.5731321850,\
#'W'   3.0731321850,\
#'K'   3.4266855756) nomirror
EOF

echo "$arrow" "$isi" "$plotplate" >> $TEMPGLT
echo "$ender" >> $TEMPGLT

gnuplot $TEMPGLT

dimensi=$(convert ${TEMPDIR}/hasil.jpg -fuzz 5% -transparent white sparse-color:-|sed -e 's/ /\n/g'|awk -F ',' 'BEGIN{a=0; b=0;aa=10000;bb=10000}{if (a<$1) a=$1; if ($1<aa) aa=$1;  if (b<$2) b=$2; if (bb>$2) bb=$2 }END{print a-(10-a%10)"x"b-bb+(10-b%10)"+"aa-(30+aa%10)"+"bb-(10-aa%10)}')
convert ${TEMPDIR}/hasil.jpg -crop $dimensi ${TEMPDIR}/hasil.jpg


for i in eps jpg; do
  mv ${TEMPDIR}/hasil.$i ${fOut}.${i};done

#rm -rf ${TEMPGLT}
#rm -rf ${TEMPDIR}
exit
