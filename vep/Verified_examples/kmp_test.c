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
#define ARRAY_SIZE 50

struct bpf_map_def SEC("maps") proxy_map = {
	.type = BPF_MAP_TYPE_HASH,
	.key_size = sizeof(unsigned long),	
	.value_size = sizeof(unsigned long),			
	.max_entries = 1000,
};

SEC("xdp")
int bpf_prog1(struct xdp_md *ctx)
{ 
    char p[ARRAY_SIZE];
    long long next[ARRAY_SIZE];
    long long n = ctx -> data;
    /* for (int i = 0; i < ARRAY_SIZE;++i) next[i] = ((i + 517) >> 2) % 100;
    int x = next[1];
    int m = 1;
    for (int i = 0; i < x ; ++ i) m = (m + next[i]); 
     bpf_printk("Value:%d\n", m);
     */
    long long i = 0, j = -1;
    next[0] = -1;
    /*Inv :  forall i, -1 <= next[i] < i*/
    while (i < n)
    {
      if (j == -1 || p[i] == p[j])
      {
         ++i; ++j; next[i] = j;
      }
      else j = next[j];
    }
    
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