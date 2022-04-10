#!/bin/bash
FROM=$1
if [ -d $FROM ]; then
    TO=$(pwd)
    pushd $FROM
      ./sync.sh $TO
    popd
else
    echo "$FROM is not a directory"
fi
