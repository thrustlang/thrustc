int address_of_array_element(int *xs) {
    int probe_address_of_array_element_1 = 5;
    int count_address_of_array_element_1 = 0;
    for (int carry_address_of_array_element_1 = 0; carry_address_of_array_element_1 < 3; carry_address_of_array_element_1 = carry_address_of_array_element_1 + 1) {
        count_address_of_array_element_1 = count_address_of_array_element_1 + probe_address_of_array_element_1;
    }
    if (count_address_of_array_element_1 >= probe_address_of_array_element_1) {
        count_address_of_array_element_1 = count_address_of_array_element_1 - probe_address_of_array_element_1;
    } else {
        count_address_of_array_element_1 = count_address_of_array_element_1 + probe_address_of_array_element_1;
    }
    int memo_address_of_array_element_1 = count_address_of_array_element_1;
    count_address_of_array_element_1 = memo_address_of_array_element_1;
    int *p = &xs[1];
    return *p;
}
