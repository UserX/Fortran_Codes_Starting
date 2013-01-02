    !  LogarithmicAndquareRootApproximations.f90 
    !
    !  FUNCTIONS:
    !  LogarithmicAndquareRootApproximations - Entry point of console application.
    !

    !****************************************************************************
    !
    !  PROGRAM: LogarithmicAndquareRootApproximations
    !  PROGRAMMER: LAPSUS
    !  DATE: 1/02/2013
    !  Please do not use the program and say its your own, give credit where credit is due.
    !  Everyone is free to use the code as long as credit is given to the PROGRAMMER
    !  PURPOSE:  Numerical approximation to logarithmic and square roots.
    !
    !****************************************************************************

    program LogarithmicAndquareRootApproximations

    IMPLICIT NONE
    INTEGER :: i, N_end, N_first, N_end_old, count, count_old
    DOUBLE PRECISION :: err, err_old, y, x, answer, answer_old, taylo, wanted_error
    !****************************************************************************
    WRITE(*,*), 'PROGRAMMER: LAPSUS'
    WRITE(*,*)
    WRITE(*,*), 'What is the number you want the Log and Sqrt of,'
    WRITE(*,*), 'the maximum number of iterations,'
    WRITE(*,*), 'and the error wanted:'
    WRITE(*,*), '(each input separated by ENTER)'
    WRITE(*,*)
    READ(*,*), y, N_end, wanted_error
    WRITE(*,*)
    answer = 0
    err =1
    !y = 19
    x = -(y-1)/(y+1)
    N_first = 1
    N_end_old = N_end
    !N_end = 50
    count = 0
1   DO i=N_first,N_end

    err_old = err
    taylo = -((-1)**i-1)*((x)**i)/i
    answer_old = answer
    answer = answer + taylo
    err = ABS(answer-answer_old)*2
    IF (ABS(err_old-err).LE.wanted_error) THEN
        GO TO 2
    ENDIF
3   IF (ABS(err_old-err).LE.wanted_error) THEN
        WRITE(*,*), 'Expected number of iterations:',N_end
        WRITE(*,*), 'Error asked for:',wanted_error
        WRITE(*,*)
        WRITE(*,*), 'The Log, sqrt, Actual error and iterations of',y
        WRITE(*,*), 'is'
        WRITE(*,*), -answer
        WRITE(*,*), EXP(.5*-answer)
        WRITE(*,*), err
        WRITE(*,*), i
        PAUSE
        STOP
    ELSEIF ((i.EQ.N_end) .AND. (ABS(err_old-err).GT.wanted_error)) THEN
        count_old = count
        count = count + 1




        N_first = N_end + 1
        N_end = N_end + 10
        GO TO 1
2       WRITE(*,*), 'Could not find an answer within the given number of iterations'
        WRITE(*,*), 'but the number of iterations where increased by', count*10
        WRITE(*,*), 'and an answer was found before reaching', N_end
        WRITE(*,*), 'iterations'
        WRITE(*,*),
        GO TO 3

    ENDIF
    ENDDO
    PAUSE


    end program LogarithmicAndquareRootApproximations

