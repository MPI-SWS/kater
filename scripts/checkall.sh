#!/bin/bash

# Get binary's full path
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
KATER="${KATER:-$DIR/../src/kater}"

echo 'Running correct tests...'
echo '------------------------------------------------------------'
for t in ${DIR}/../tests/correct/*.kat
do
    printf "%-50s: " \
	   "${t##*/}"
    output=`"${KATER}" ${t} >/dev/null 2>&1 `
    if test "$?" -ne 0
    then
    	echo "Error"
    	problem=1
    else
	echo "OK"
    fi
done
echo ''

echo 'Running wrong tests...'
echo '------------------------------------------------------------'
for t in ${DIR}/../tests/wrong/*.kat
do
    printf "%-50s: " \
	   "${t##*/}"
    output=`"${KATER}" ${t} >/dev/null 2>&1`
    if test "$?" -ne 6
    then
    	echo "Error"
    	problem=1
    else
	echo "OK"
    fi
done
echo ''

if [[ "${problem}" -eq 1 ]]
then
    echo '-----------------------------------'
    echo '--- UNEXPECTED TESTING RESULTS! ---'
    echo '-----------------------------------'
    exit 1
fi
echo '--- Testing proceeded as expected'
