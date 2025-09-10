#!/usr/bin/env bash
set -euo pipefail

OUT_FILE="src/All.hs"
TMP_FILE="$(mktemp)"
trap 'rm -f "$TMP_FILE"' EXIT

modules=()
while IFS= read -r -d '' file; do
  [[ "$file" == "$OUT_FILE" ]] && continue
  rel="${file#src/}"
  mod="${rel%.hs}"
  mod="${mod//\//.}"
  modules+=("$mod")
done < <(find src -type f -name '*.hs' -print0)

IFS=$'\n' modules=($(sort -u <<<"${modules[*]}"))
unset IFS

mkdir -p src

{
  echo "-- AUTO-GENERATED"
  echo "module All ("
  for i in "${!modules[@]}"; do
    sep=$([[ $i -lt $((${#modules[@]} - 1)) ]] && echo "," || echo "")
    echo "  module ${modules[$i]}$sep"
  done
  echo ") where"
  echo
  for m in "${modules[@]}"; do
    echo "import $m"
  done
  echo
} > "$TMP_FILE"

if [[ ! -f "$OUT_FILE" ]] || ! cmp -s "$TMP_FILE" "$OUT_FILE"; then
  mv "$TMP_FILE" "$OUT_FILE"
  echo "Wrote $OUT_FILE with ${#modules[@]} modules."
else
  echo "No changes. $OUT_FILE is up to date."
fi
