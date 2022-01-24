#include<stdio.h>

#define BITS 19
#define SEGS 5264
typedef unsigned long int U;
typedef __uint128_t UL;
#define LSGN "l"
#define _STR(_X) #_X
#define _PRN(_X, _B) do { if (dot) { dot = 0;\
	printf("%"LSGN"u.", (_X) / (a / 1000)); printf("%.*"LSGN"u", (_B) - 3, (_X) % (a / 1000));\
} else printf("%."_STR(_B)LSGN"u", _X); } while (0)
#define PRN(_X) _PRN(_X, BITS)
#define INVL (UL)-1
#define INV (U)-1
#define CR(_M, _N, _D, _CN, _CD, _U)\
UL _M(void) {\
	static U f[((SEGS) + 1) * (_U) + 1]; static unsigned int c = sizeof(f) / sizeof(f[0]) - 1;\
	static unsigned char kont = 0; unsigned int b, g; UL d;\
	switch (kont) { case 0: kont = 1;\
	for (b = 0; b++ - c; f[b] = a / 100 / (_CD) * (_CN));\
	for (; d = 0, g = (c * 2 - 1) * (_D), c;) {\
		for (b = c; d += (UL)f[b] * a, f[b] = (U)(d % g), d /= g, g -= 2 * (_D), --b; d *= b * (_N));\
		c -= (_U); return d; case 1:;\
	} kont = 2; default: return INVL; }\
} typedef void nop
U a = 1; CR(cc1, 1, 25, 7, 1, 12); CR(cc2, 9, 3125, 474, 5, 7);

int main() {
	U b, c = INV, d = 0; UL x, y; unsigned char dot = 1;
	for (b = (BITS) + 1; --b; a *= 10);
	while (((x = cc1()) != INVL) && ((y = cc2()) != INVL)) {
		d += (U)((x + y) / a); b = (U)((x + y) % a); if (c != INV) PRN(c); c = d; d = b;
	} putchar('\n'); return 0;
}
