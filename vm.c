#include <assert.h>
#include <fcntl.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

#include "vm.h"


#define D_OP_MASK		0xF0000000
#define D_OP_SHIFT		28
#define D_R1_MASK		0x0FFFC000
#define D_R1_SHIFT		14
#define D_R2_MASK		0x00003FFF
                    
#define S_OP_MASK		0x0F000000
#define S_OP_SHIFT		24
#define S_C_OP_MASK		0x00E00000
#define S_C_OP_SHIFT	21
#define S_R1_MASK		0x00003FFF

#define D_S_OP			0x0
#define D_ADD_OP		0x1
#define D_SUB_OP		0x2
#define D_MUL_OP		0x3
#define D_DIV_OP		0x4
#define D_OUTPUT_OP		0x5
#define D_PHI_OP		0x6
                    	
#define S_NO_OP			0x0
#define S_CMPZ_OP		0x1
#define S_SQRT_OP		0x2
#define S_COPY_OP		0x3
#define S_INPUT_OP		0x4

#define C_LTZ_OP		0x0
#define C_LEZ_OP		0x1
#define C_EQZ_OP		0x2
#define C_GEZ_OP		0x3
#define C_GTZ_OP		0x4


#if 0
#define DEBUG(format, ...) \
	do { \
		fprintf(stderr, format, ##__VA_ARGS__); \
		fflush(stderr); \
	} while (0)
#define ASSERT(cond) assert(x)
#else
#define DEBUG(format, ...)
#define ASSERT(cond)
#endif


void halt(const char *msg) {
	perror(msg);
	exit(1);
}


uint32_t read_binary(const char *path, uint8_t **dst_p) {
	int fd;
	struct stat info;
	uint8_t *src, *dst;
	if ((fd = open(path, O_RDONLY)) == -1)
		halt("open");
	if (fstat(fd, &info) == -1)
		halt("fstat");
	if ((src = mmap(0, info.st_size, PROT_READ, MAP_SHARED, fd, 0)) == (uint8_t *)-1)
		halt("mmap");
	if (!(dst = malloc(info.st_size)))
		halt("malloc");
	memcpy(dst, src, info.st_size);
	munmap(src, info.st_size);
	close(fd);
	*dst_p = dst;
	return info.st_size;
}


void parse_binary(struct state *s, const uint8_t *buf, uint32_t length) {
	const uint8_t *buf_i = buf, *end = buf + length;
	uint32_t *code_i;
	double *data_i;
	ASSERT(!(length % (sizeof(uint32_t) + sizeof(double))));
	s->size = length / (sizeof(uint32_t) + sizeof(double));
	ASSERT(s->size <= MAX_SIZE);
	code_i = s->code;
	data_i = s->data;
	while (1) {
		if (buf_i == end)
			break;
		*(data_i++) = *(double *)buf_i;
		*(code_i++) = *(uint32_t *)(buf_i + sizeof(double));
		buf_i += (sizeof(uint32_t) + sizeof(double));
		if (buf_i == end)
			break;
		*(code_i++) = *(uint32_t *)buf_i;
		*(data_i++) = *(double *)(buf_i + sizeof(uint32_t));
		buf_i += (sizeof(uint32_t) + sizeof(double));
	}
}


struct state *new(const char *path, uint32_t conf) {
	struct state *s;
	uint8_t *buf;
	uint32_t length;
	if (!(s = malloc(sizeof(struct state))))
		halt("malloc");
	s->status = 0;
	s->input[CONF_INPUT] = conf;
	s->max_output = 0;
	length = read_binary(path, &buf);
	parse_binary(s, buf, length);
	free(buf);
	return s;
}

struct state *copy(const struct state *s1) {
	struct state *s2;
	if (!(s2 = malloc(sizeof(struct state))))
		halt("malloc");
	memcpy(s2, s1, sizeof(struct state));
	return s2;
}


inline double read_data(const struct state *s, uint32_t i) {
	if (i >= s->size)
		return 0.0;
	return s->data[i];
}

inline void write_data(struct state *s, uint32_t i, double v) {
	ASSERT(i < s->size);
	s->data[i] = v;
}

inline void output_value(struct state *s, uint32_t p, double v) {
	ASSERT(p < MAX_SIZE);
	s->output[p] = v;
	if (p > s->max_output)
		s->max_output = p;
}

inline double input_value(const struct state *s, uint32_t p) {
	ASSERT(p < MAX_SIZE);
	return s->input[p];
}


void unsafe_set_inputs(struct state *s, double dx, double dy) {
	s->input[DX_INPUT] = dx;
	s->input[DY_INPUT] = dy;
}


void unsafe_run_step(struct state *s) {
	uint32_t i;
	for (i = 0; i < s->size; i++) {
		uint32_t inst = s->code[i];
		uint32_t d_op = (inst & D_OP_MASK) >> D_OP_SHIFT;
		if (d_op != D_S_OP) {
			if (d_op == D_ADD_OP) {
				uint32_t d_r1 = (inst & D_R1_MASK) >> D_R1_SHIFT;
				uint32_t d_r2 = inst & D_R2_MASK;
				double v1 = read_data(s, d_r1);
				double v2 = read_data(s, d_r2);
				write_data(s, i, v1 + v2);
			}
			else if (d_op == D_SUB_OP) {
				uint32_t d_r1 = (inst & D_R1_MASK) >> D_R1_SHIFT;
				uint32_t d_r2 = inst & D_R2_MASK;
				double v1 = read_data(s, d_r1);
				double v2 = read_data(s, d_r2);
				write_data(s, i, v1 - v2);
			}
			else if (d_op == D_MUL_OP) {
				uint32_t d_r1 = (inst & D_R1_MASK) >> D_R1_SHIFT;
				uint32_t d_r2 = inst & D_R2_MASK;
				double v1 = read_data(s, d_r1);
				double v2 = read_data(s, d_r2);
				write_data(s, i, v1 * v2);
			}
			else if (d_op == D_DIV_OP) {
				uint32_t d_r2 = inst & D_R2_MASK;
				double v2 = read_data(s, d_r2);
				if (v2 == 0.0)
					write_data(s, i, 0.0);
				else {
					uint32_t d_r1 = (inst & D_R1_MASK) >> D_R1_SHIFT;
					double v1 = read_data(s, d_r1);
					write_data(s, i, v1 / v2);
				}
			}
			else if (d_op == D_OUTPUT_OP) {
				uint32_t d_r1 = (inst & D_R1_MASK) >> D_R1_SHIFT;
				uint32_t d_r2 = inst & D_R2_MASK;
				double v2 = read_data(s, d_r2);
				output_value(s, d_r1, v2);
			}
			else if (d_op == D_PHI_OP) {
				if (s->status) {
					uint32_t d_r1 = (inst & D_R1_MASK) >> D_R1_SHIFT;
					double v1 = read_data(s, d_r1);
					write_data(s, i, v1);
				}
				else {
					uint32_t d_r2 = inst & D_R2_MASK;
					double v2 = read_data(s, d_r2);
					write_data(s, i, v2);
				}
			}
			else {
				fprintf(stderr, "d_op == %d\n", d_op);
				assert(0);
			}
		}
		else {
			uint32_t s_op = (inst & S_OP_MASK) >> S_OP_SHIFT;
			if (s_op == S_CMPZ_OP) {
				uint32_t s_r1 = inst & S_R1_MASK;
				double v1 = read_data(s, s_r1);
				uint32_t c_op = (inst & S_C_OP_MASK) >> S_C_OP_SHIFT;
				if (c_op == C_LTZ_OP)
					s->status = v1 < 0.0 ? 1 : 0;
				else if (c_op == C_LEZ_OP)
					s->status = v1 <= 0.0 ? 1 : 0;
				else if (c_op == C_EQZ_OP)
					s->status = v1 == 0.0 ? 1 : 0;
				else if (c_op == C_GEZ_OP)
					s->status = v1 >= 0.0 ? 1 : 0;
				else if (c_op == C_GTZ_OP)
					s->status = v1 > 0.0 ? 1 : 0;
				else {
					fprintf(stderr, "c_op == %d\n", c_op);
					assert(0);
				}
			}
			else if (s_op == S_SQRT_OP) {
				uint32_t s_r1 = inst & S_R1_MASK;
				double v1 = read_data(s, s_r1);
				write_data(s, i, fabs(sqrt(v1)));
			}
			else if (s_op == S_COPY_OP) {
				uint32_t s_r1 = inst & S_R1_MASK;
				double v1 = read_data(s, s_r1);
				write_data(s, i, v1);
			}
			else if (s_op == S_INPUT_OP) {
				uint32_t s_r1 = inst & S_R1_MASK;
				write_data(s, i, input_value(s, s_r1));
			}
			else if (s_op != S_NO_OP) {
				fprintf(stderr, "s_op == %d\n", s_op);
				assert(0);
			}
		}
	}
}


void unsafe_run_n_steps(struct state *s, uint32_t n) {
	uint32_t i;
	for (i = 0; i < n; i++)
		unsafe_run_step(s);
}


struct state *run_step(const struct state *s1, double dx, double dy) {
	struct state *s2;
	s2 = copy(s1);
	unsafe_set_inputs(s2, dx, dy);
	unsafe_run_step(s2);
	return s2;
}


struct state* run_n_steps(const struct state *s1, uint32_t n, double dx, double dy) {
	struct state *s2;
	s2 = copy(s1);
	unsafe_set_inputs(s2, dx, dy);
	unsafe_run_n_steps(s2, n);
	return s2;
}


uint32_t get_max_output(const struct state *s) {
	return s->max_output;
}


double get_output(const struct state *s, uint32_t p) {
	ASSERT(p < s->max_output);
	return s->output[p];
}
