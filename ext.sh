#!/bin/bash
#Create_Time 2019-08-06
#use: small_wei
#查找并,批量修改文件后缀
#后缀为 .txt 修改为 .log
ff=`ls |grep .md`
for f in $ff
do
	echo $f
	echo ''>$f 
done
