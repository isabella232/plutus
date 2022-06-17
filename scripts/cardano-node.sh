#!/usr/bin/env bash

set -euo pipefail
node_bin_dir=../cardano-node-bin
node_dir=../cardano-node-files
node_zipped="cardano-node-1.34.1-linux.tar.gz"
node_config_files=(
  "mainnet-config.json"
  "mainnet-byron-genesis.json"
  "mainnet-shelley-genesis.json"
  "mainnet-alonzo-genesis.json"
  "mainnet-topology.json"
)

set -x
mkdir -p "$node_bin_dir"
wget -nc https://hydra.iohk.io/build/13065769/download/1/$node_zipped -P "$node_bin_dir"

tar zxvf "$node_bin_dir"/$node_zipped -C "$node_bin_dir"

mkdir -p "$node_dir"
cd "$node_dir"
for x in "${node_config_files[@]}"; do
  if ! [ -f "$x" ]; then
    curl -O -J https://hydra.iohk.io/build/7370192/download/1/"$x"
  fi
done
cd -

"$node_bin_dir"/cardano-node run \
  --config "$node_dir"/mainnet-config.json \
  --topology "$node_dir"/mainnet-topology.json \
  --database-path "$node_dir"/db/ \
  --socket-path "$node_dir"/db/node.socket \
  --host-addr 127.0.0.1 \
  --port 1337
