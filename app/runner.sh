#!/bin/bash
date
for d in $(awk '{print $2}' ~/opt/workList); do
  echo =======================================================
  cd $d
  
  tQ=$(head -2 jcf.sekirei.sh|tail -1|awk '{print $3}')
  echo Checking $tQ at $d

  stat=$(tail -2 llmf|grep CPU) 
  if [ -n "$stat" ] ; then 
    echo Work of $tQ is finished at $stat, skip into next job
    continue
  fi

  tinQ=$(/home/k0044/k004407/bin/cek.sh|grep "$tQ")
  if [ -z "$tinQ" ]; then
    echo submit $tQ 
    /opt/pbs/issp/bin/qsub jcf.sekirei.sh
  else
    echo $tQ still running 
  fi
done
