#./app/genSuperCell.hs -i sample/9009852neutron.ncs.n.cif -s 1x1x1 \
# -c sample/cpvo/celldm0.ncs.angstrom -b "0.0 0.5 0.5  0.5 0.0 0.5  0.5 0.5 0.0" \
# -m "1.178000 1.178000 1.178000" -s "Co Co Ni1 Ni2 CoTd CoTd S S S S S S S S" \
# -t sample/ctrl.ncsib.template $1 -v "0.25770 0.25770 0.257701"

./app/genSuperCell.hs -i $1 -s 1x1x1 \
 -c sample/cpvo/celldm0.ncs.angstrom -b "0.0 0.5 0.5  0.5 0.0 0.5  0.5 0.5 0.0" \
 -m "1.178000 1.178000 1.178000" \
 -t sample/ctrl.ncsib.template $2 -v "0.25770 0.25770 0.257701"

#./app/genSuperCell.hs -i sample/9009852neutron.ncs.n.cif -s 1x1x1 \
# -c sample/cpvo/celldm0.ncs.angstrom \
# -m "1.178000 1.178000 1.178000" \
# -t sample/ctrl.ncsib.template -l -v "0.25770 0.25770 0.257701"

# "0.25770:0.25770:0.25770"
# ./perintah.hs 0.25770:0.25770:0.25770
