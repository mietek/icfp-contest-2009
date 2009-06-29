#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "vm.h"


int main(int argc, char **argv) {
	char *path;
	struct state *s;
	uint32_t conf, max;
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
		path = argv[2];
	}
	else
		path = argv[1];
	scanf("%d", &conf);
	s = new(path, conf);
	while (1) {
		uint32_t steps, i;
		double dx, dy;
		scanf("%d", &steps);
		if (feof(stdin) || !steps)
			break;
		scanf("%lf %lf", &dx, &dy);
		unsafe_set_inputs(s, dx, dy);
		unsafe_run_n_steps(s, steps);
		max = get_max_output(s);
		if (human) {
			double out[max];
			for (i = 0; i <= max; i++)
				out[i] = get_output(s, i);
			printf("score          = %lf\n", out[0]);
			printf("fuel           = %lf\n", out[1]);
			printf("my absolute p  = (%lf, %lf)\n", out[2], out[3]);
			printf("my absolute r  = %lf\n", sqrt(out[2] * out[2] + out[3] * out[3]));
			if (conf >= 1001 && conf <= 1004)
				printf("target r       = %lf\n", out[4]);
			else {
				printf("his absolute p = (%lf, %lf)\n", out[2] - out[4], out[3] - out[5]);
				printf("his absolute r = %lf\n", sqrt((out[2] - out[4]) * (out[2] - out[4]) + (out[3] - out[5]) * (out[3] - out[5])));
				printf("distance       = %lf\n", sqrt((out[4] * out[4]) + (out[5] * out[5])));
				if (conf >= 4001 && conf <= 4004)
					printf("update vm!!1\n");
			}
			printf("\n");
		}
		else {
			printf("%d\n", max + 1);
			for (i = 0; i <= max; i++)
				printf("%d %lf\n", i, get_output(s, i));
		}
	}
	free(s);
	return 0;
}
