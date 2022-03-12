#include<gmp.h>

int main(int argc, char *argv[])
{
	mp_bitcnt_t e = 0;
	mpf_t a, b, d, t;
	mpf_set_default_prec(33219281);

	mpf_init_set_ui(a, 2);
	mpf_sqrt(a, a);
	mpf_init_set_ui(b, 1);
	mpf_init_set_ui(d, 2);
	mpf_init(t);

	while (e < 20) {
		mpf_sub(t, a, b);
		mpf_mul(t, t, t);
		mpf_mul_2exp(t, t, e++);
		mpf_sub(d, d, t);
		mpf_add(t, a, b);
		mpf_mul(b, a, b);
		mpf_sqrt(b, b);
		mpf_div_2exp(a, t, 1);
	}

	mpf_mul(t, a, a);
	mpf_mul_2exp(t, t, 2);
	mpf_div(t, t, d);
	gmp_printf("%.1000000Ff\n", t);

	mpf_clears(a, b, d, t, NULL);
	return 0;
}
