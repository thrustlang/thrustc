#define BASE3 10
#define MULBASE3(x) ((x) * BASE3)
#define ADDMB3(x) (MULBASE3(x) + BASE3)
int addmb_val(void) {
    return ADDMB3(5);
}
