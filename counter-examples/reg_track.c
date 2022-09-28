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

struct bpf_map_def SEC("maps") proxy_map = {
	.type = BPF_MAP_TYPE_HASH,
	.key_size = sizeof(unsigned long),	
	.value_size = sizeof(unsigned long),			
	.max_entries = 1000,
};


SEC("xdp")
int func(struct xdp_md *ctx)
{
    char buffer[30];
    char buffer1[1];
    __u32 p;
    p = bpf_get_prandom_u32() % 30;
    
    // #pragma nounroll   // which maybe a bug in prevail
    for (int i = 0;i<30;++i) {
        buffer[i] = bpf_get_prandom_u32() % 2;
    }
    buffer1[0] = buffer[p];

    // The following should fail verification since it asks the helper
    // to write past the end of the stack.
    return bpf_get_current_comm(buffer1, 20);
}

char _license[] SEC("license") = "GPL";



    // Bytecode of the above program
    //  0:  b7 07 00 00 00 00 00 00 r7 = 0
    //  1:  85 00 00 00 07 00 00 00 call 7
    //  2:  bf 06 00 00 00 00 00 00 r6 = r0
    //  3:  bf a8 00 00 00 00 00 00 r8 = r10
    //  4:  07 08 00 00 e2 ff ff ff r8 += -30
    //  5:  0f 78 00 00 00 00 00 00 r8 += r7
    //  6:  85 00 00 00 07 00 00 00 call 7
    //  7:  57 00 00 00 01 00 00 00 r0 &= 1
    //  8:  73 08 00 00 00 00 00 00 *(u8 *)(r8 + 0) = r0
    //  9:  07 07 00 00 01 00 00 00 r7 += 1
    // 10:  15 07 01 00 1e 00 00 00 if r7 == 30 goto +1 <LBB0_2>
    // 11:  05 00 f7 ff 00 00 00 00 goto -9 <LBB0_1>
    // 12:  67 06 00 00 20 00 00 00 r6 <<= 32
    // 13:  77 06 00 00 20 00 00 00 r6 >>= 32
    // 14:  bf 61 00 00 00 00 00 00 r1 = r6
    // 15:  37 01 00 00 1e 00 00 00 r1 /= 30
    // 16:  27 01 00 00 1e 00 00 00 r1 *= 30
    // 17:  1f 16 00 00 00 00 00 00 r6 -= r1
    // 18:  bf a1 00 00 00 00 00 00 r1 = r10
    // 19:  07 01 00 00 e2 ff ff ff r1 += -30
    // 20:  0f 61 00 00 00 00 00 00 r1 += r6
    // 21:  71 11 00 00 00 00 00 00 r1 = *(u8 *)(r1 + 0)
    // 22:  73 1a e1 ff 00 00 00 00 *(u8 *)(r10 - 31) = r1
    // 23:  bf a1 00 00 00 00 00 00 r1 = r10
    // 24:  07 01 00 00 e1 ff ff ff r1 += -31
    // 25:  b7 02 00 00 14 00 00 00 r2 = 20
    // 26:  85 00 00 00 10 00 00 00 call 16
    // 27:  95 00 00 00 00 00 00 00 exit