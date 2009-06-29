#include <stdint.h>


#define MAX_SIZE		16384

#define DX_INPUT		2
#define DY_INPUT		3
#define CONF_INPUT		16000


struct state {
	uint32_t code[MAX_SIZE];
	double data[MAX_SIZE];
	uint32_t size;
	uint32_t status;
	double input[MAX_SIZE];
	double output[MAX_SIZE];
	uint32_t max_output;
};


struct state *new(const char *path, uint32_t conf);
struct state *copy(const struct state *s1);
struct state *run_step(const struct state *s1, double dx, double dy);
struct state *run_n_steps(const struct state *s1, uint32_t n, double dx, double dy);
uint32_t get_max_output(const struct state *s);
double get_output(const struct state *s, uint32_t p);

void unsafe_set_inputs(struct state *s, double dx, double dy);
void unsafe_run_step(struct state *s);
void unsafe_run_n_steps(struct state *s, uint32_t n);
