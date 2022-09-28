/* Copyright (c) 2017 Facebook
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2 of the GNU General Public
 * License as published by the Free Software Foundation.
 *
 * BPF program to set initial congestion window and initial receive
 * window to 40 packets and send and receive buffers to 1.5MB. This
 * would usually be done after doing appropriate checks that indicate
 * the hosts are far enough away (i.e. large RTT).
 *
 * Use "bpftool cgroup attach $cg sock_ops $prog" to load this BPF program.
 */

#include <linux/bpf.h>
#include <linux/if_ether.h>
#include <linux/if_packet.h>
#include <linux/ip.h>
#include <linux/socket.h>
#include <bpf/bpf_helpers.h>
#include <bpf/bpf_endian.h>


#define SOL_SOCKET 1
#define SO_ACCEPTCONN 30
#define SO_BROADCAST 6
#define SO_DONTROUTE 5
#define SO_ERROR 4
#define SO_KEEPALIVE 9
#define SO_LINGER 13
#define SO_OOBINLINE 10
#define SO_RCVBUF 8
#define SO_RCVLOWAT 18
#define SO_REUSEADDR 2
#define SO_SNDBUF 7
#define SO_SNDLOWAT 19
#define SO_TYPE 3
#define SOL_TCP		6

SEC("sockops")
int bpf_iw(struct bpf_sock_ops *skops)
{
	int bufsize = 1500000;
	int rwnd_init = 40;
	int iw = 40;
	int rv = 0;
	int op;

	/* For testing purposes, only execute rest of BPF program
	 * if neither port numberis 55601
	 */
	if (bpf_ntohl(skops->remote_port) != 55601 &&
	    skops->local_port != 55601) {
		skops->reply = -1;
		return 1;
	}

	op = (int) skops->op;



	/* Usually there would be a check to insure the hosts are far
	 * from each other so it makes sense to increase buffer sizes
	 */
	switch (op) {
	case BPF_SOCK_OPS_RWND_INIT:
		rv = rwnd_init;
		break;
	case BPF_SOCK_OPS_TCP_CONNECT_CB:
		/* Set sndbuf and rcvbuf of active connections */
		rv = bpf_setsockopt(skops, SOL_SOCKET, SO_SNDBUF, &bufsize,
				    sizeof(bufsize));
		rv += bpf_setsockopt(skops, SOL_SOCKET, SO_RCVBUF,
				     &bufsize, sizeof(bufsize));
		break;
	case BPF_SOCK_OPS_ACTIVE_ESTABLISHED_CB:
		rv = bpf_setsockopt(skops, SOL_TCP, TCP_BPF_IW, &iw,
				    sizeof(iw));
		break;
	case BPF_SOCK_OPS_PASSIVE_ESTABLISHED_CB:
		/* Set sndbuf and rcvbuf of passive connections */
		rv = bpf_setsockopt(skops, SOL_SOCKET, SO_SNDBUF, &bufsize,
				    sizeof(bufsize));
		rv +=  bpf_setsockopt(skops, SOL_SOCKET, SO_RCVBUF,
				      &bufsize, sizeof(bufsize));
		break;
	default:
		rv = -1;
	}

	skops->reply = rv;
	return 1;
}
char _license[] SEC("license") = "GPL";