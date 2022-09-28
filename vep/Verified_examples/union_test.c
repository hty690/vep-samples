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

#define MAP_SIZE 4000000
#define Node_SIZE 10
#define Edge_SIZE 100

struct bpf_map_def SEC("maps") proxy_map = {
	.type = BPF_MAP_TYPE_HASH,
	.key_size = sizeof(unsigned long),	
	.value_size = sizeof(unsigned long) * (Edge_SIZE * 3 + 2),			
	.max_entries = 10,
};

SEC("xdp")
int bpf_prog1(struct xdp_md *ctx)
{ 
    unsigned int fa[Node_SIZE],size[Node_SIZE];
    int* pattern;
    int key = 0,x,y,value,prev = -1,ans = 0;
    pattern = bpf_map_lookup_elem(&proxy_map, &key);
    if (pattern == NULL)
        return XDP_DROP;
    int n = pattern[0], m = pattern[1];
    if (n >= Node_SIZE) return XDP_DROP;
    if (m >= Edge_SIZE) return XDP_DROP;
    for (int i = 1;i<=n;++i) fa[i] = i,size[i] = 1;
    //  Forall i , fa[i] = i /\ size[i] = 1 
    for (int i = 1;i<=m;++i)
    {
      x = pattern[i * 3 - 1];
      y = pattern[i * 3];
      value = pattern[i * 3 + 1];
      if (prev != -1 && prev > value) return XDP_DROP;
      prev = value;
      while (x != fa[x]) {x = fa[x];}
      while (y != fa[y]) {y = fa[y];}
      if (x != y)
      {
        if (size[x] < size[y]) fa[x] = y,size[x] += size[y] ;
        else fa[y] = x , size[y] += size[x];
        ans += value;
      }
    }
    bpf_printk("%d\n",ans);
    return XDP_PASS;
    
}

char _license[] SEC("license") = "GPL";