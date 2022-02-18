#!/bin/bash

for i in $(ls *.firrtl)
do
	echo $i
	/home/lucas/Repos/chisel-template/firrtl/utils/bin/firrtl -i $i --emission-options=disableMemRandomization,disableRegisterRandomization
done
