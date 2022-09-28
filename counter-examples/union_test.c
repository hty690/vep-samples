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
	.key_size = sizeof(__u64),	
	.value_size = sizeof(__u64) * (Edge_SIZE * 3 + 2),			
	.max_entries = 10,
};

SEC("xdp")
int bpf_prog1(struct xdp_md *ctx)
{ 
    __u64 fa[Node_SIZE],size[Node_SIZE];
    __u64* pattern;
    __u64 key = 0,x,y,ans = 0;
    pattern = bpf_map_lookup_elem(&proxy_map, &key);
    if (pattern == NULL)
        return XDP_DROP;
    __u64 n = pattern[0], m = pattern[1];
    if (n >= Node_SIZE) return XDP_DROP;
    if (m >= Edge_SIZE) return XDP_DROP;
    for (int i = 1;i<=n;++i) fa[i] = i,size[i] = 1;
    //  Forall i , fa[i] = i /\ size[i] = 1 
    for (int i = 1;i<=m;++i)
    {
      x = pattern[i * 3 - 1];
      y = pattern[i * 3];
      if (i != 1 && pattern[i * 3 + 1] < pattern[i * 3 - 2]) return XDP_DROP;
      if (x < 1 || x > n) return XDP_DROP;
      if (y < 1 || y > n) return XDP_DROP;
      while (x != fa[x]) {x = fa[x];}
      while (y != fa[y]) {y = fa[y];} 
      if (x != y)
      {
        if (size[x] < size[y]) fa[x] = y,size[x] += size[y] ;
        else fa[y] = x , size[y] += size[x];
        ans += pattern[i * 3 + 1];
      }
    }
    bpf_printk("%lld\n",ans);
    return XDP_PASS;
    
}

char _license[] SEC("license") = "GPL";



 /*
    can pass the prevail
    unsigned int n = 5;
    int i = 0, j = -1;
    
    while (i < n)
    {
      if (j == -1 || (i ^ j) % 107 == 0)
      {
         ++i; ++j;
      }
      else if (j > 1  && ( (i ^ j) % 93  == 0))  j = j - 2;
      else j = j - 1;

    }
    return XDP_PASS;
  */
 
  /*  
    can't pass the prevail
    char p[ARRAY_SIZE] = "hello";
    int next[ARRAY_SIZE];
    unsigned int n = 5;
    int i = 0, j = -1;
    next[0] = -1;
    while (i < n)
    {
      if (j == -1 || p[i] == p[j])
      {
         ++i; ++j; next[i] = j;
      }
      else j = next[j];
    }
    return XDP_PASS;
  */