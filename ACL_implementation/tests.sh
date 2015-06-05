#!/bin/bash
# bash script


#CONSTANTS

# Ficheiro de load
LOADFILE=load.lisp
# Directorio base do projecto
BASEDIR=$(pwd)
# Directorio dos testes
INDIR=$BASEDIR/tests/inputs
# Directorio de saida
OUTDIR=$BASEDIR/tests/out
# Directorio dos resultados esperados
EXPECTEDDIR=$BASEDIR/tests/outputs

rm -rf $OUTDIR
mkdir $OUTDIR


cp $BASEDIR/$LOADFILE $TEMPDIR/$LOADFILE

clear

i=0
passed=0
failed=0
dif=""

echo "=============================================================================================="
echo "== Starting tests now"
echo "=============================================================================================="

if [ "$1" ==  "" ] # Run all tests
  then
    for file in $INDIR/**/*.lisp
    do
	outputfile=${file:${#INDIR}}
	outputfile=${outputfile:1}
	outputfile=${outputfile%%.*}
	outputdir=${outputfile%/*}
	filename=$(basename $file)
	filename=${filename%%.*}
	cp -f $file $BASEDIR/testFile.lisp
	mkdir -p $OUTDIR/$outputdir
	sbcl < $file &> $OUTDIR/$outputfile.output
	diff <(tail -n +2 $OUTDIR/$outputfile.output) <(tail -n +2 $EXPECTEDDIR/$outputfile.lisp.output) >/dev/null
	dif=$?
	if  [ "$dif" -eq "0" ]; then
	    echo "==Passed test $filename"
	    passed=`expr $passed + 1`
	else
	    #echo "=============================================================================================="
	    echo "==Failed test $filename"
	    #echo "=============================================================================================="
	    #diff -b $OUTDIR/$filename.output $EXPECTEDDIR/$filename.output
	    #echo "=============================================================================================="
	    echo " "
        exit
	    failed=`expr $failed + 1`
	fi
	i=`expr $i + 1`

    done
else # run specific test
  filename=$(basename $1)
  filename=${filename%%.*}
  $PROJDIR/mayfly -g $INDIR/$filename.mf -o $OUTDIR/$filename.asm
  yasm -felf $OUTDIR/$filename.asm -o $TEMPDIR/$filename.o
  ld -o $TEMPDIR/$filename $TEMPDIR/$filename.o -lrts
  $TEMPDIR/$filename > $OUTDIR/$filename.out
  diff -b $OUTDIR/$filename.out $EXPECTEDDIR/$filename.out >/dev/null
  dif=$?
  if  [ "$dif" -eq "0" ]; then
      echo "==Passed test $filename"
      passed=`expr $passed + 1`
  else
      #echo "=============================================================================================="
      echo "==Failed test $filename"
      #echo "=============================================================================================="
      #diff -b $OUTDIR/$filename.out $EXPECTEDDIR/$filename.out
      #echo "=============================================================================================="
      echo " "
      failed=`expr $failed + 1`
  fi
  i=`expr $i + 1`
fi

echo "**********************************************************************************************"
echo "**** Tests terminated"
echo "**********************************************************************************************"
echo "**** PASSED: $passed"
echo "**** FAILED: $failed"
echo "**** TOTAL: $i"
echo "**********************************************************************************************"
