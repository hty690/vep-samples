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
#define ARRAY_SIZE 10

struct bpf_map_def SEC("maps") proxy_map = {
	.type = BPF_MAP_TYPE_HASH,
	.key_size = sizeof(unsigned long),	
	.value_size = sizeof(unsigned long) * (ARRAY_SIZE + 1),			
	.max_entries = 5,
};

SEC("xdp")
int bpf_prog1(struct xdp_md *ctx)
{ 
    // char p[ARRAY_SIZE] = "hello";
    unsigned int lookup[ARRAY_SIZE + 1][ARRAY_SIZE + 1] = {};
     lookup[0][0] = 1;
     int* pattern;
     int key = 0;
    pattern = bpf_map_lookup_elem(&proxy_map, &key);
    if (pattern == NULL)
        return XDP_PASS;
    // Only '*' can match with empty string
    for (int j = 1; j <= ARRAY_SIZE; j++)
         if (*(pattern + j) == 1){
            //  lookup[0][j] = j;
             key = lookup[0][j] + 1;
             bpf_printk("%d\n", lookup[0][j]);
        }
    
    /* for (int j = 1; j < ARRAY_SIZE; j++) {
      // if (pattern[j] == 1)
      //   lookup[0][j] = lookup[0][j - 1];
		    // bpf_printk("1\n");
        (pattern[j] == 1) ? (lookup[0][j] = lookup[0][j - 1]) : ({});
    }
    */
    

    
 
    // fill the table in bottom-up fashion
    // for (int i = 1; i <= ARRAY_SIZE; i++) {
    //     for (int j = 1; j <= ARRAY_SIZE; j++) {
    //         // Two cases if we see a '*'
    //         // a) We ignore ‘*’ character and move
    //         //    to next  character in the pattern,
    //         //     i.e., ‘*’ indicates an empty sequence.
    //         // b) '*' character matches with ith
    //         //     character in input
    //         if (pattern[j - 1] == 1)
    //             lookup[i][j]
    //                 = lookup[i][j - 1] || lookup[i - 1][j];
 
    //         // Current characters are considered as
    //         // matching in two cases
    //         // (a) current character of pattern is '?'
    //         // (b) characters actually match
    //         else if (pattern[j - 1] == 0
    //                  )
    //             lookup[i][j] = lookup[i - 1][j - 1];
 
    //         // If characters don't match
    //         else
    //             lookup[i][j] = 0;
    //     }
    // }
    // bpf_printk("%d\n", lookup[ARRAY_SIZE][ARRAY_SIZE]);

    return XDP_PASS;
    
}

char _license[] SEC("license") = "GPL";


