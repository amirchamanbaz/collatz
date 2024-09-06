!Find the starting number within a range of numbers for which the collatz sequence length is the highest 
! Algorithm takes ~4 seconds on an old laptop to find this number below 100 million, 60ms for below 1 million
! Compile with "gfortran collatzsequence.f90 -o collatzsequence -O3"
    
    program collatzsequence
    use omp_lib !parallel computing hardly improves the algorithm speed and might introduce overhead, you can omit this if you want
    use iso_fortran_env
    implicit none
    integer :: i, startnumb, res, chain
    integer(int64) :: n
    integer, dimension(200000000) :: arr !Set limit for the search, default here is 2x10^8. Maximum size of the array depends on the limits of your machine 

    n = 1
    startnumb = 0
    res = 0
    arr = 0

    !$omp parallel do private(i, n, chain) shared(arr) reduction(max:res)
    do i = 200000000, 1, -1 !Time can be improved if you set the lower limit higher than 1 and make assumptions where the starting number with max sequence will occur
        chain = 1
        if (arr(i) .ne. 0) then !If starting integer (i) has been previously visited, skip i. If it has been previously visited, we don't need to compute this part again
            cycle
        endif

        n = i
        do while (n .ne. 1) 
            chain = chain + 1

            if (n .le. size(arr) .and. arr(n) .le. chain) then !If current sequence length leading to a number is higher than previously found, update this sequence length
                arr(n) = chain
            else if (n .le. size(arr) .and. arr(n) .gt. chain) then !If we have found a longer sequence leading to a number previously, halt the generation of the sequence
                n = 1
                exit
            endif

            if (mod(n, 2) .eq. 0) then !Logic to generate collatz sequence 
                n = n / 2
            else
                n = 3 * n + 1
            endif
        end do

        !$omp critical
        if (chain .gt. res) then !if new sequence is longer than the one we found, update max sequence length (res) and store the starting number
            res = chain
            startnumb = i
        endif
        !$omp end critical

    end do
    !$omp end parallel do

    print *, startnumb
    !print *, res
end program collatzsequence
