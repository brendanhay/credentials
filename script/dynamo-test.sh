#!/usr/bin/env bash

set -e

store="$1"

if [ -z "$store" ]; then
    echo "Usage: run <STORE>"
    exit 1
fi

region="eu-central-1"

send() {
    local mode="$1"
    local args="$2"

    credentials $mode -r $region -u $store $args --output echo
}

selects() {
    local name="$1"
    local expect="$2"
    local context="$3"

    local actual="$(credentials select -r $region -u $store --name $name $context --output echo)"

    if [ "$expect" != "$actual" ]; then
        send "list"
        echo ""
        echo "Test failed for $name, expected: $expect, actual: $actual"
        send "select" "-l debug --name $name $context"
        echo ""
        exit 1
    fi
}

inserts() {
    local __revision=$1
    local name="$2"
    local secret="$3"
    local context="$4"

    local revision=$(credentials insert -r $region -u $store --name $name -s "$secret" $context --output echo)
    selects "$name" "$secret" "$context"

    eval $__revision="'$revision'"
}

send "teardown" "-f"
echo ""
send "setup"

echo ""
echo "Insert series ..."

inserts foo1 "foo" "secret"
inserts foo2 "foo" "not so secret"
inserts foo3 "foo" "something"
inserts foo4 "foo" "bother"
inserts bar1 "bar" "rah rah rah"
inserts bar2 "bar" "nothing"
inserts baz1 "baz" "supercalifragilistic"
inserts baz2 "baz" "anything"

echo "Delete revision $foo3"

send "delete" "--name foo --revision $foo3 -f"
selects "foo" "bother"

echo "Delete revision $foo4"

send "delete" "--name foo --revision $foo4 -f"
selects "foo" "not so secret"

echo "Insert with context ..."

inserts bar3 "bar" "longer secret '-c a=b -c this=notthis'"

send "list"

echo "Done."
