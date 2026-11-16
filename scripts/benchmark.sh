#!/usr/bin/env bash
# Runs GossipSimulator.fsx over a range of network sizes and adds one CSV line per run to results/results.csv.
#
#   scripts/benchmark.sh
#   DOTNET=/path/to/dotnet RUN_TIMEOUT=7200 scripts/benchmark.sh
#
# Edit the seeds and sizes below to change the sweep. Every run uses --verify. A run that does not converge or fails
# a check is recorded with its status and verify columns saying so. A run that prints no CSV line is recorded with
# status "timeout" if it took longer than RUN_TIMEOUT seconds (default 3600; needs the timeout command, as on Linux)
# and "crashed" otherwise.
#
# Runs that are already in results/results.csv are skipped, so an interrupted sweep continues where it stopped when
# the script is started again. Delete a line to run it again, or the file to start over.
set -euo pipefail

seeds="1 2 3"

# Network sizes for each algorithm and topology. 3D and imp3D round sizes up to perfect cubes, so these are cubes.
# With these lists and three seeds (150 runs), single runs timed on a 12-core desktop add up to about 1.9 hours.
# The longest, push-sum on 1,000,000 imp3D nodes, took 8.5 minutes.
gossip_full="10 100 1000 10000 100000 1000000"
gossip_3d="27 125 1000 8000 64000 216000 1000000"
gossip_line="10 100 1000 10000 20000"
gossip_imp3d="27 125 1000 8000 64000 216000 1000000"
push_sum_full="10 100 1000 10000 100000 1000000"
push_sum_3d="27 125 1000 3375 8000 15625"
push_sum_line="10 20 50 100 200 300"
push_sum_imp3d="27 125 1000 8000 64000 216000 1000000"

DOTNET="${DOTNET:-dotnet}"
RUN_TIMEOUT="${RUN_TIMEOUT:-3600}"

cd "$(dirname "$0")/.."

# This also checks that $DOTNET works before results.csv is touched.
header=$("$DOTNET" fsi GossipSimulator.fsx --csv-header)
mkdir -p results
out=results/results.csv
if [ ! -s "$out" ]; then
  echo "$header" > "$out"
elif [ "$(head -n 1 "$out")" != "$header" ]; then
  echo "$out has other columns than GossipSimulator.fsx prints; move it away to start a new sweep" >&2
  exit 1
fi
columns=$(echo "$header" | awk -F, '{ print NF }')

if command -v timeout > /dev/null; then
  limited=1
else
  limited=0
  echo "The timeout command was not found, so runs have no time limit" >&2
fi

simulate() {
  if [ "$limited" = 1 ]; then
    timeout "$RUN_TIMEOUT" "$DOTNET" fsi GossipSimulator.fsx "$@"
  else
    "$DOTNET" fsi GossipSimulator.fsx "$@"
  fi
}

# Whether results.csv has a line for this algorithm, topology, size and seed
recorded() {
  awk -F, -v a="$1" -v t="$2" -v n="$3" -v s="$4" '
    NR == 1 { for (i = 1; i <= NF; i++) column[$i] = i; next }
    $(column["algorithm"]) == a && $(column["topology"]) == t && $(column["nodes_requested"]) == n &&
      $(column["seed"]) == s { found = 1; exit }
    END { exit !found }' "$out"
}

# A line for a run that printed none: the run's settings and status, with the other columns empty
missing_row() {
  awk -F, -v OFS=, -v t="$1" -v a="$2" -v n="$3" -v s="$4" -v st="$5" 'NR == 1 {
    value["topology"] = t; value["algorithm"] = a; value["nodes_requested"] = n; value["seed"] = s
    value["status"] = st
    for (i = 1; i <= NF; i++) $i = value[$i]
    print
    exit
  }' "$out"
}

run() {
  local algorithm=$1 topology=$2 sizes=$3 nodes seed output row code status
  for nodes in $sizes; do
    for seed in $seeds; do
      if recorded "$algorithm" "$topology" "$nodes" "$seed"; then
        continue
      fi
      echo "$(date +%H:%M:%S) $algorithm $topology $nodes, seed $seed" >&2
      code=0
      output=$(simulate "$nodes" "$topology" "$algorithm" --seed "$seed" --verify --csv) || code=$?
      row=$(printf '%s\n' "$output" | awk -F, -v n="$columns" 'NF == n' | tail -n 1)
      if [ -n "$row" ]; then
        echo "$row" >> "$out"
        if [ "$code" -ne 0 ]; then
          echo "  exit code $code (see the status and verify columns)" >&2
        fi
      else
        if [ "$code" -eq 124 ]; then status=timeout; else status=crashed; fi
        echo "  exit code $code without a CSV line; recorded as $status" >&2
        missing_row "$topology" "$algorithm" "$nodes" "$seed" "$status" >> "$out"
      fi
    done
  done
}

run gossip full "$gossip_full"
run gossip 3D "$gossip_3d"
run gossip line "$gossip_line"
run gossip imp3D "$gossip_imp3d"
run push-sum full "$push_sum_full"
run push-sum 3D "$push_sum_3d"
run push-sum line "$push_sum_line"
run push-sum imp3D "$push_sum_imp3d"

echo "Wrote $out" >&2
