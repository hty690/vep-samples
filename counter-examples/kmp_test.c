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
    /*
    Assert : 
    Exists ctx n p next i j : val , l1 l2 : list val , l : struct 
    PROP (n = (l -> data); l2[0] = -1; i = 0 ; j = -1;)
    Local (Temp _ctx ctx ; Temp _n n ; Temp _p p ; Temp _next next ; Temp _i i; Temp _j _j)
    Sep (Packet ctx l ;  Memory_block p l1 ; Memory_block next l2;)
    */
    
    /*
    Inv : 
    Exists ctx n p next i j m : val , l1 l2 : list val , l : struct 
    PROP (n = (l -> data); 0 <= i < n; -1 <= j < n; Forall k , 0 <= k < i -> 0 <= l2[k] < k; m < 2 * i - j)
    Local (Temp _ctx ctx ; Temp _n n ; Temp _p p ; Temp _next next ; Temp _i i; Temp _j j)
    Sep (Packet ctx l ;  Memory_block p l1 n ; Memory_block next l2 n; Time_spend m) 
    */

    while (i < n)
    {
      if (j == -1 || p[i] == p[j])
      {
         ++i; ++j; next[i] = j;
      }
      else j = next[j];
    }
    O(\sigma (next[i])) = O (\sigma i) 
    /*
    Post 
    */
    Assert => Inv 
    Exe (Inv /\ condition = true) ==> Inv 
    Inv /\ condition = false ===> Post;
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