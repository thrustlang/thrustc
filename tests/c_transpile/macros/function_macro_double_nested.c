#define ONE2() (1)
#define ADDONE2(x) ((x) + ONE2())
#define ADD2(x) (ADDONE2(x) + ONE2())
int nested_seven(int x) {
    return ADD2(x);
}
