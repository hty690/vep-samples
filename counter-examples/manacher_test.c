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
#define ARRAY_SIZE 20
static int min(int a, int b)
{
  return a < b ? a : b;
}

static int max(int a, int b)
{
  return a > b ? a : b;
}

/*
  Here if we do not add static , the preveil and linux-verifier can't not find out which section need it to verify.
  But this is a part that can be optimized.
  
*/
SEC("xdp")
int bpf_prog1(struct xdp_md *ctx)
{ 
    char p[ARRAY_SIZE];
    int r[ARRAY_SIZE << 1];
    int n = ctx -> data;
    char s[ARRAY_SIZE << 1];
    int m = 0, R = -1,ans = 1;
    int id = -1;
    s[0] = '$'; s[++m] = '#';
    for (int i = 0; i < n; ++i) {
        s[++m] = p[i];
        s[++m] = '#';
    }
    s[++m] = '?';
    for (int i = 0; i < m; ++i) {
      r[i] = R > i ? min(r[2*id - i], R - i + 1) : 1;
      while (s[i-r[i]] == s[i+r[i]]) r[i]++;
      if (i + r[i] > R) {
        R = i + r[i] - 1;
        id = i;
      }
      ans = max(ans , r[i]);
    }
    bpf_printk("Value:%d\n", ans);
    return XDP_PASS;
}

char _license[] SEC("license") = "GPL";

/*
  can't pass the verifier

  void *data_end = (void *)(long)ctx->data_end;
    void *data = (void *)(long)ctx->data;

    if (data + sizeof(int) > data_end)
        return 1;

    int value = *(int*)data;
    *(int*)data = value + 1;

    bpf_skb_change_head(ctx, 4, 0);

    value = *(int*)data;
    *(int*)data = value + 1;
    return 0;



*/