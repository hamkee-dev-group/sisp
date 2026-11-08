#!/bin/sh
# Every lib module must survive every other module being loaded after it.
# SISP resolves global helper names at call time, so an accidental duplicate
# can silently replace a helper beneath an already-loaded public function.

set -eu

script_dir=$(CDPATH= cd -- "$(dirname "$0")" && pwd)
root=$(CDPATH= cd -- "$script_dir/.." && pwd)
sisp="$root/src/sisp"
driver=$(mktemp /tmp/sisp-lib-coexistence.XXXXXX.lsp)
output=$(mktemp /tmp/sisp-lib-coexistence.XXXXXX.out)
trap 'rm -f "$driver" "$output"' EXIT HUP INT TERM

definitions()
{
	sed -nE \
		-e 's/^\(define \(([^][()[:space:]]+).*/\1/p' \
		-e 's/^\(define ([^][()[:space:]]+).*/\1/p' "$1"
}

# Every .lsp under lib/ is a module.
modules=$(find "$root/lib" -type f -name '*.lsp' | sort)
module_count=$(printf '%s\n' "$modules" | wc -l | tr -d ' ')
pair_count=0

for first in $modules
do
	first_relative=${first#"$root/"}
	for second in $modules
	do
		[ "$first" = "$second" ] && continue
		second_relative=${second#"$root/"}
		{
			printf '(load "%s")\n' "$first_relative"
			index=0
			for name in $(definitions "$first")
			do
				printf '(define lib-coexistence-snapshot-%s %s)\n' \
					"$index" "$name"
				index=$((index + 1))
			done
			printf '(load "%s")\n' "$second_relative"
			index=0
			for name in $(definitions "$first")
			do
				printf '(if (equal lib-coexistence-snapshot-%s %s) t ' \
					"$index" "$name"
				printf '(error "LIB NAME COLLISION AFTER %s" (quote %s)))\n' \
					"$second_relative" "$name"
				index=$((index + 1))
			done
			printf '(quit 0)\n'
		} >"$driver"
		if ! (cd "$root" && "$sisp" "$driver" </dev/null >"$output" 2>&1)
		then
			printf 'FAIL: %s then %s\n' "$first_relative" "$second_relative"
			cat "$output"
			exit 1
		fi
		pair_count=$((pair_count + 1))
	done
done

printf 'PASS: %s lib modules coexist in all %s ordered pairs\n' \
	"$module_count" "$pair_count"
