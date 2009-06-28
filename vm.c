#include <assert.h>
#include <fcntl.h>
#include <math.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>


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

#define MAX_SIZE		16384

#define DX_INPUT		2
#define DY_INPUT		3
#define CONF_INPUT		16000


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


uint32_t the_code[MAX_SIZE];
double the_data[MAX_SIZE];
uint32_t the_size;
uint32_t the_status = 0;
double the_input[MAX_SIZE];
double the_output[MAX_SIZE];
uint32_t the_max_output = 0;


void halt(char *msg) {
	perror(msg);
	exit(1);
}


uint32_t read_binary(char *path, uint8_t **dst_p) {
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


void extract_frames(uint8_t *buf, uint32_t length) {
	uint8_t *buf_i = buf, *end = buf + length;
	uint32_t *code_i;
	double *data_i;
	ASSERT(!(length % (sizeof(uint32_t) + sizeof(double))));
	the_size = length / (sizeof(uint32_t) + sizeof(double));
	ASSERT(the_size <= MAX_SIZE);
	code_i = the_code;
	data_i = the_data;
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


inline double read_data(uint32_t i) {
	if (i >= the_size)
		return 0.0;
	return the_data[i];
}

inline void write_data(uint32_t i, double v) {
	ASSERT(i < the_size); // TODO: will this be enough or do we have to resize memory?
	the_data[i] = v;
}

inline void output_value(uint32_t p, double v) {
	ASSERT(p < MAX_SIZE);
	the_output[p] = v;
	if (p > the_max_output)
		the_max_output = p;
}

inline double input_value(uint32_t p) {
	ASSERT(p < MAX_SIZE);
	return the_input[p];
}


void execute_frames() {
	uint32_t i;
	for (i = 0; i < the_size; i++) {
		uint32_t inst = the_code[i];
		uint32_t d_op = (inst & D_OP_MASK) >> D_OP_SHIFT;
		if (d_op != D_S_OP) {
			if (d_op == D_ADD_OP) {
				uint32_t d_r1 = (inst & D_R1_MASK) >> D_R1_SHIFT;
				uint32_t d_r2 = inst & D_R2_MASK;
				double v1 = read_data(d_r1);
				double v2 = read_data(d_r2);
				write_data(i, v1 + v2);
			}
			else if (d_op == D_SUB_OP) {
				uint32_t d_r1 = (inst & D_R1_MASK) >> D_R1_SHIFT;
				uint32_t d_r2 = inst & D_R2_MASK;
				double v1 = read_data(d_r1);
				double v2 = read_data(d_r2);
				write_data(i, v1 - v2);
			}
			else if (d_op == D_MUL_OP) {
				uint32_t d_r1 = (inst & D_R1_MASK) >> D_R1_SHIFT;
				uint32_t d_r2 = inst & D_R2_MASK;
				double v1 = read_data(d_r1);
				double v2 = read_data(d_r2);
				write_data(i, v1 * v2);
			}
			else if (d_op == D_DIV_OP) {
				uint32_t d_r2 = inst & D_R2_MASK;
				double v2 = read_data(d_r2);
				if (v2 == 0.0)
					write_data(i, 0.0);
				else {
					uint32_t d_r1 = (inst & D_R1_MASK) >> D_R1_SHIFT;
					double v1 = read_data(d_r1);
					write_data(i, v1 / v2);
				}
			}
			else if (d_op == D_OUTPUT_OP) {
				uint32_t d_r1 = (inst & D_R1_MASK) >> D_R1_SHIFT;
				uint32_t d_r2 = inst & D_R2_MASK;
				double v2 = read_data(d_r2);
				output_value(d_r1, v2);
			}
			else if (d_op == D_PHI_OP) {
				if (the_status) {
					uint32_t d_r1 = (inst & D_R1_MASK) >> D_R1_SHIFT;
					double v1 = read_data(d_r1);
					write_data(i, v1);
				}
				else {
					uint32_t d_r2 = inst & D_R2_MASK;
					double v2 = read_data(d_r2);
					write_data(i, v2);
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
				double v1 = read_data(s_r1);
				uint32_t c_op = (inst & S_C_OP_MASK) >> S_C_OP_SHIFT;
				if (c_op == C_LTZ_OP)
					the_status = v1 < 0.0 ? 1 : 0;
				else if (c_op == C_LEZ_OP)
					the_status = v1 <= 0.0 ? 1 : 0;
				else if (c_op == C_EQZ_OP)
					the_status = v1 == 0.0 ? 1 : 0;
				else if (c_op == C_GEZ_OP)
					the_status = v1 >= 0.0 ? 1 : 0;
				else if (c_op == C_GTZ_OP)
					the_status = v1 > 0.0 ? 1 : 0;
				else {
					fprintf(stderr, "c_op == %d\n", c_op);
					assert(0);
				}
			}
			else if (s_op == S_SQRT_OP) {
				uint32_t s_r1 = inst & S_R1_MASK;
				double v1 = read_data(s_r1);
				write_data(i, fabs(sqrt(v1)));
			}
			else if (s_op == S_COPY_OP) {
				uint32_t s_r1 = inst & S_R1_MASK;
				double v1 = read_data(s_r1);
				write_data(i, v1);
			}
			else if (s_op == S_INPUT_OP) {
				uint32_t s_r1 = inst & S_R1_MASK;
				write_data(i, input_value(s_r1));
			}
			else if (s_op != S_NO_OP) {
				fprintf(stderr, "s_op == %d\n", s_op);
				assert(0);
			}
		}
	}
}


int main(int argc, char **argv) {
	uint8_t *buf;
	uint32_t length;
	uint32_t conf;
	uint8_t human = 0;
	if (argc < 2 || argc > 3) {
		fprintf(stderr, "Usage: %s [-h] binary\n", argv[0]);
		return 1;
	}
	if (argc == 3) {
		if (strcmp(argv[1], "-h")) {
			fprintf(stderr, "Usage: %s [-h] binary\n", argv[0]);
			return 1;
		}
		human = 1;
		length = read_binary(argv[2], &buf);
	}
	else
		length = read_binary(argv[1], &buf);
	extract_frames(buf, length);
	free(buf);
	scanf("%d", &conf);
	the_input[CONF_INPUT] = conf;
	while (1) {
		uint32_t steps, i;
		double dx, dy;
		scanf("%d", &steps);
		if (feof(stdin) || !steps)
			break;
		scanf("%lf %lf", &dx, &dy);
		the_input[DX_INPUT] = dx;
		the_input[DY_INPUT] = dy;
		for (i = 0; i < steps; i++)
			execute_frames();
		if (human) {
			printf("score          = %lf\n", the_output[0]);
			printf("fuel           = %lf\n", the_output[1]);
			printf("my absolute p  = (%lf, %lf)\n", the_output[2], the_output[3]);
			printf("my absolute r  = %lf\n", sqrt(the_output[2] * the_output[2] + the_output[3] * the_output[3]));
			if (conf >= 1001 && conf <= 1004)
				printf("target r       = %lf\n", the_output[4]);
			else {
				printf("his absolute p = (%lf, %lf)\n", the_output[2]-the_output[4], the_output[3]-the_output[5]);
				printf("his absolute r = %lf\n", sqrt((the_output[2]-the_output[4]) * (the_output[2]-the_output[4]) + (the_output[3]-the_output[5]) * (the_output[3]- the_output[5])));
				if (conf >= 4001 && conf <= 4004)
					printf("update vm!!1\n");
			}
			printf("\n");
		}
		else {
			printf("%d\n", the_max_output + 1);
			for (i = 0; i <= the_max_output; i++)
				printf("%d %lf\n", i, the_output[i]);
		}
	}
	return 0;
}
