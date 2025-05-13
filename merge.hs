mergeSort ls = mergeS ls 0 ((length ls) - 1)

--caso base
mergeSort [] = []

mergeS ls es di = nls
    where
        if es < di
        then
            meio = ((es + (di - es)) / 2); 
            mergeS ls es meio
            mergeS ls meio+1 di
            -- mescla as metades
            nls = merge ls esquerda meio direita
        else
            nls = ls

merge lista e m d = nlista
    where 
        nlista = lista ++ e ++ m ++ d


--// Função para mesclar dois subvetores
--void merge(vector<int>& arr, int esquerda, int meio, int direita) {
  --  int n1 = meio - esquerda + 1;
    --int n2 = direita - meio;

    --// Vetores temporários
    --vector<int> L(n1);
    --vector<int> R(n2);

    --// Copiando dados para os vetores temporários
    --for (int i = 0; i < n1; i++)
      --  L[i] = arr[esquerda + i];
    --for (int j = 0; j < n2; j++)
      --  R[j] = arr[meio + 1 + j];

    --// Índices iniciais dos subvetores
    --int i = 0, j = 0, k = esquerda;

    --// Mescla os vetores temporários de volta no vetor original
    --while (i < n1 && j < n2) {
      --  if (L[i] <= R[j]) {
        --    arr[k++] = L[i++];
        --} else {
          --  arr[k++] = R[j++];
       -- }
    --}

    --// Copia os elementos restantes de L, se houver
    --while (i < n1)
      --  arr[k++] = L[i++];

    --// Copia os elementos restantes de R, se houver
    --while (j < n2)
      --  arr[k++] = R[j++];
--}

--// Função principal do Merge Sort
--void mergeSort(vector<int>& arr, int esquerda, int direita) {
  --  if (esquerda < direita) {
    --    int meio = esquerda + (direita - esquerda) / 2;

      --  // Ordena as duas metades
        --mergeSort(arr, esquerda, meio);
        --mergeSort(arr, meio + 1, direita);

        --// Mescla as duas metades
        --merge(arr, esquerda, meio, direita);
   -- }
--}



