#!/bin/bash

# Get binary's full path
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
KATER="${KATER:-$DIR/../Release/kater}"
KATERFLAGS="${KATERFLAGS:-}"

# Env variables
TIMEFORMAT='%2R'

total_time=0
echo 'Running correct tests...'
echo '------------------------------------------------------------'
for t in ${DIR}/../tests/correct/*/*.kat; do
    printf "%-40s | " \
	   "${t##*/}"

    time=`{ time "${KATER}" ${KATERFLAGS} ${t} &> /dev/null; } 2>&1`
    status="$?"
    total_time=$( bc <<<"scale=2; $total_time + $time" )
    if test "$status" -ne 0
    then
    	printf "%-6s | %-8s\n" "Error" "$time"
    	problem=1
    else
    	printf "%-6s | %-8s\n" "OK" "$time"
    fi
done
echo ''

echo 'Running wrong tests...'
echo '------------------------------------------------------------'
for t in ${DIR}/../tests/wrong/*/*.kat; do
    printf "%-40s | " \
	   "${t##*/}"

    time=`{ time "${KATER}" ${KATERFLAGS} ${t} &> /dev/null; } 2>&1`
    status="$?"
    total_time=$( bc <<<"scale=2; $total_time + $time" )

    regex=".*/tests/wrong/parsing/.*"
    [[ $t =~ $regex ]] && expected_status=5 || expected_status=6
    if [[ "$status" -ne "$expected_status" ]]; then
    	printf "%-6s | %-8s\n" "Error" "$time"
    	problem=1
    else
	printf "%-6s | %-8s\n"  "OK" "$time"
    fi
done
echo ''

if [[ "${problem}" -eq 1 ]]
then
    echo '------------------------------------------------------------'
    echo '--- UNEXPECTED TESTING RESULTS: ' "${total_time}" ' ---'
    echo '------------------------------------------------------------'
    exit 1
fi
echo '--- Testing proceeded as expected: ' "${total_time}"
