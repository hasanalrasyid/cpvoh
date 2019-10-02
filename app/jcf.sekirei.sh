#!/bin/sh
#PBS -N 0.ib -j oe
#PBS -l walltime=0:30:0
#QSUB -queue i18cpu
#QSUB -node 14
#QSUB -mpi 280
###QSUB -node 12 
###QSUB -mpi 36
#QSUB -omp 1
#QSUB -place distribute 
#QSUB -over true

#########---#QSUB -node 20 
#########---#QSUB -mpi 280
###---#QSUB -node 14 
###---#QSUB -mpi 280
. /etc/profile.d/modules.sh
module load intel/18.0.3.222 intel-mpi/2018.3.222 cuda git/2.16.2

cd ${PBS_O_WORKDIR}

echo ===============================================
#env
echo ===============================================

d=`date`
echo 
echo starting date : ${d}
echo 

#gwsc 3 -np 48 ib 
echo ===============================================
echo '+gwsc started :' `date` $PBS_O_WORKDIR '+'
ulimit -s unlimited
s=llmf;i=0; while [ -f "$s" ]; do  i=$((i+1)) ;echo $i; printf -v num '%06d' $i; s="llmf_$num"; echo llmf_$num ;done
cp -f llmf llmf_$num

export MKL_NUM_THREADS=1
export OMP_NUM_THREADS=1
export MYCPU=280
#export MYCPU=54
#export MYCPU=36

/home/k0044/k004407/bin/lmchk --pr60 ib > llmchk
/home/k0044/k004407/bin/lmfa ib > llmfa
mpijob -np $MYCPU /home/k0044/k004407/bin/lmf-MPIK ib > llmf

#--#/home/k0044/k004407/bin/gwsc_continue 5 -np $MYCPU nico2o4 >& outme.$PBS_JOBID
#--#
#--##mpiexec -machinefile $PBS_NODEFILE -n $PBS_NP /home/k0044/k004407/bin/lmfa lanio3c >&llmfa
#--#/home/k0044/k004407/bin/job_pdos nico2o4 -np $MYCPU >& outme.job_pdos.$PBS_JOBID 
#--#/home/k0044/k004407/bin/job_tdos nico2o4 -np $MYCPU >& outme.job_tdos.$PBS_JOBID
#--#/home/k0044/k004407/bin/job_band nico2o4 -np $MYCPU >& outme.job_band.$PBS_JOBID
echo '-gwsc finished :' `date` $PBS_O_WORKDIR '-'




#export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/home/k0044/k004407/opt/libs/issp
######## these jobs already ran in gwsc during iter > 0 

#job_pdos ib -np 48 
#job_tdos ib -np 48  
#job_band ib -np 48 

d=`date`
echo 
echo finishing date : ${d}
echo 
