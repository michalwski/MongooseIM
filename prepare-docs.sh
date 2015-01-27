#!/bin/bash
set -x
set -e
set -o pipefail

TAG="1.5"
DATE=2015-01-27

add_dates() {
    mv "$1" "$DATE-$1"
}

relativise_links() {
    gsed -i -e 's/(\(.*.md\))/\(..\/\1)/g' $1
}

disable_liquid_templates() {
    gsed -i '1s/\(.*\)/{% raw %}\1/' $1
    echo '{% endraw %}' >> $1 
}

main() {
    pushd $TAG/_posts
    for f in *.md; do
        relativise_links $f
        disable_liquid_templates $f
        add_dates $f
    done
    popd
}

main
