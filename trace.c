#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <unistd.h>


#define TEAM_ID			159

#define DX_INPUT		2
#define DY_INPUT		3
#define CONF_INPUT		16000


void halt(char *msg) {
	perror(msg);
	exit(1);
}


void write_int(uint32_t v) {
	if (write(1, &v, sizeof(uint32_t)) == -1)
		halt("write");
}


void write_double(double v) {
	if (write(1, &v, sizeof(double)) == -1)
		halt("write");
}


void write_frame(uint32_t i, double d) {
	write_int(i);
	write_double(d);
}


void write_head(uint32_t conf, double dx, double dy) {
	write_int(0xCAFEBABE);
	write_int(TEAM_ID);
	write_int(conf);
	write_int(0);
	write_int(3);
	write_frame(DX_INPUT, dx);
	write_frame(DY_INPUT, dy);
	write_frame(CONF_INPUT, conf);
}


void write_step(uint32_t all_steps, double last_dx, double last_dy, double dx, double dy) {
	if (dx != last_dx) {
		if (dy != last_dy) {
			write_int(all_steps);
			write_int(2);
			write_frame(DX_INPUT, dx);
			write_frame(DY_INPUT, dy);
		}
		else {
			write_int(all_steps);
			write_int(1);
			write_frame(DX_INPUT, dx);
		}
	}
	else {
		if (dy != last_dy) {
			write_int(all_steps);
			write_int(1);
			write_frame(DY_INPUT, dy);
		}
	}
}


void write_foot(uint32_t all_steps) {
	write_int(all_steps);
	write_int(0);
}


int main() {
	uint32_t conf, all_steps = 0, steps;
	double last_dx, last_dy, dx, dy;
	scanf("%d", &conf);
	scanf("%d", &steps);
	scanf("%lf %lf", &dx, &dy);
	write_head(conf, dx, dy);
	while (1) {
		all_steps += steps;
		last_dx = dx;
		last_dy = dy;
		scanf("%d", &steps);
		if (feof(stdin) || !steps)
			break;
		scanf("%lf %lf", &dx, &dy);
		write_step(all_steps, last_dx, last_dy, dx, dy);
	}
	write_foot(all_steps);
	return 0;
}
