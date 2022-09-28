#include <linux/bpf.h>
#include <bpf/bpf_endian.h>
#include <bpf/bpf_helpers.h>
#include <linux/if_ether.h>
#include <linux/if_packet.h>
#include <linux/if_vlan.h>
#include <linux/ip.h>
#include <linux/in.h>
#include <linux/tcp.h>
#include <linux/udp.h>

#define MAX_LENGTH 500

struct bpf_map_def SEC("maps") proxy_map = {
        .type = BPF_MAP_TYPE_HASH,
        .key_size = sizeof(__u32),
        .value_size = sizeof(__u32),
        .max_entries = 2,
};



SEC("bpf_prog")
int bpf_prog1(struct xdp_md *ctx) {
    __u32 key, *value;
    __u64 tmp;
    key = 0;
    value = bpf_map_lookup_elem(&proxy_map, &key);
    if (value == NULL)
        return XDP_DROP;
    if (*value > 50)
        return XDP_DROP;
    tmp = bpf_get_prandom_u32();
    for (int i = 0; i < *value; i++) {
        tmp += bpf_get_prandom_u32();
    }
    bpf_printk("%d\n", tmp);
    return XDP_PASS;
    
}