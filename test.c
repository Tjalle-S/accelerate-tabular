// Sparse-sparse
for (pB1 = B1_pos[0]; pB1 < B1_pos[1]; pB1++) {
  i = B1_idx[pB1];
  for (pB2 = B2_pos[pB1]; pB2 < B2_pos[pB1+1]; pB2++) {
    j = B2_idx[pB2];
    val = B_vals[pB2];
    printf(“B(%d,%d) = %f”, i, j, val);
  }
}

// Dense-sparse
for (i = 0; pB1 < N1; i++) {
  for (pB2 = B2_pos[i]; pB2 < B2_pos[i+1]; pB2++) {
    j = B2_idx[pB2];
    val = B_vals[pB2];
    printf(“B(%d,%d) = %f”, i, j, val);
  }
}

// Sparse-dense
for (pB1 = B1_pos[0]; pB1 < B1_pos[1]; pB1++) {
  i = B1_idx[pB1];
  for (j = 0; j < N2; j++) {
    val = B_vals[N2 * pB1 + j];
    printf(“B(%d,%d) = %f”, i, j, val);
  }
}

// Dense-dense
for (i = 0; i < N1; i++) {
  for (j = 0; j < N2; j++) {
    val = B_vals[N2 * i + j];
    printf(“B(%d,%d) = %f”, i, j, val);
  }
}
