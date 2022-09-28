// Copyright (c) Prevail Verifier contributors.
// SPDX-License-Identifier: MIT
#include <linux/bpf.h>
#include <string.h>
#include <bpf/bpf_endian.h>
#include <bpf/bpf_helpers.h>
#include <linux/if_ether.h>
#include <linux/if_packet.h>
#include <linux/if_vlan.h>
#include <linux/ip.h>
#include <linux/in.h>
#include <linux/tcp.h>
#include <linux/udp.h>

typedef unsigned int uint32_t;
typedef unsigned long uint64_t;

struct bpf_map_def SEC("maps") map1 = {
	.type = BPF_MAP_TYPE_ARRAY,
  .key_size = sizeof(int),
  .value_size = sizeof(uint64_t),
  .max_entries = 1,
};

struct bpf_map_def SEC("maps") map2 = {
	.type = BPF_MAP_TYPE_ARRAY,
  .key_size = sizeof(int),
  .value_size = sizeof(uint32_t),
  .max_entries = 2,
};

struct ctx;

int func(struct ctx* ctx)
{
    uint32_t rand32 = bpf_get_prandom_u32();
    struct bpf_map_def* map = (rand32 & 1) ? &map1 : &map2;

    int key = 10;
    uint64_t* value = (uint64_t*)bpf_map_lookup_elem(map, &key);
    if (value == 0)
        return 0;

    // The following is safe since both maps have the same value size.
    return (int)*value;
}