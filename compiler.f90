program compiler
    implicit none

    enum, bind(c)
        enumerator :: stmt_arith, stmt_asgn_goto, stmt_assign, stmt_comp_goto
        enumerator :: stmt_continue, stmt_dim, stmt_do, stmt_goto, stmt_read
        enumerator :: stmt_print, stmt_if, stmt_format, stmt_stop
    end enum

    integer, parameter :: MAX_ID = 31, MAX_VARS = 200, MAX_DIM = 100

    call main()
contains
    integer function insert(len, arr, elem)
        integer,          intent(in)    :: len
        character(len=*), intent(inout) :: arr(:)
        character(len=*), intent(in)    :: elem
        character(len=1024)             :: buf
        integer                         :: i

        call trimw(elem, buf)

        insert = 1

        do i = 1, len
            if (arr(i) == buf) then
                insert = 0
                exit
            end if
        end do

        if (insert == 1) then
            arr(len + 1) = buf
        end if
    end function insert

    subroutine sort(len, arr)
        integer, intent(in)    :: len
        integer, intent(inout) :: arr(len)
        integer                :: tmp, i, j

        do i = 1, len - 1
            do j = i, 1, -1
                if (arr(j + 1) < arr(j)) then
                    tmp        = arr(j + 1)
                    arr(j + 1) = arr(j)
                    arr(j)     = tmp
                end if
            end do
        end do
    end subroutine sort

    integer function find(len, arr, elem)
        integer, intent(in) :: len
        integer, intent(in) :: arr(len)
        integer, intent(in) :: elem
        integer             :: min, mid, max

        max = len
        min = 1

        do
            if (max < min) then
                find = 0
                exit
            end if

            mid = min + (max - min) / 2

            if (arr(mid) == elem) then
                find = mid
                exit
            else if (arr(mid) < elem) then
                min = mid + 1
            else
                max = mid - 1
            end if
        end do
    end function find

    subroutine rep_char(c, n, out)
        character(len=1), intent(in)  :: c
        integer,          intent(in)  :: n
        character(len=n), intent(out) :: out
        integer                       :: i

        do i = 1, n
            out(i:i) = c(1:1)
        end do
    end subroutine rep_char

    subroutine replace_char(str, c, with)
        character(len=*), intent(inout) :: str
        character(len=1), intent(in)    :: c
        character(len=*), intent(in)    :: with
        character(len=2 * len(str))     :: buf
        integer                         :: i, j

        buf = ''
        j = 1

        do i = 1, len(str)
            if (str(i:i) == c) then
                buf(j:j + len(with) - 1) = with
                j = j + len(with)
            else
                buf(j:j) = str(i:i)
                j = j + 1
            end if
        end do

        str = buf
    end subroutine replace_char

    integer function match_paren(line)
        character(len=*), intent(in) :: line
        integer                      :: parens, i

        parens = 0
        match_paren = 0

        do i = 1, len(line)
            if (line(i:i) == '(') then
                parens = parens + 1
            else if (line(i:i)==')') then
                parens = parens - 1
            end if

            if (parens == 0) then
                match_paren = i
                exit
            end if
        end do
    end function match_paren

    logical function is_comment(line)
        character(len=*), intent(in) :: line

        is_comment = (line(1:1) .eq. 'c') .or. (line(1:1) .eq. 'C')
    end function is_comment

    logical function is_cont(line)
        character(len=*), intent(in) :: line

        is_cont = .not.(line(6:6) == ' ' .or. line(6:6) == '0')
    end function is_cont

    subroutine trimw(str, out)
        character(len=*), intent(in)  :: str
        character(len=*), intent(out) :: out
        integer                       :: i

        do i = 1, len(str)
            if (str(i:i) == ' ') then
                cycle
            else
                exit
            end if
        end do

        out = str(i:)
    end subroutine trimw

    subroutine nospace(buf)
        character(len=*), intent(inout) :: buf
        integer                         :: i, j

        j = 1

        do i = 1, len(buf)
            if (buf(i:i) .ne. '') then
                buf(j:j) = buf(i:i)
                j = j + 1
            end if
        end do
    end subroutine nospace

    integer function count_lines(filename)
        character(len=*), intent(in) :: filename
        character(len=1024)          :: line
        integer                      :: stat, uin = 0, line_count = 0

        open (uin, file=filename)

        do
            read (uin, '(a)', iostat=stat) line

            if (stat < 0) exit

            if (.not. (is_comment(line) .or. is_cont(line) .or. line == '')) then
                line_count = line_count + 1
            end if
        end do

        count_lines = line_count
    end function count_lines

    integer function count_labels(nlines, lines)
        integer,          intent(in) :: nlines
        character(len=*), intent(in) :: lines(nlines)
        integer                      :: nlabels = 0, i

        do i = 1, nlines
            if ((index(lines(i), 'FORMAT') .eq. 0) .and. (lines(i)(1:6) .ne. '')) then
                nlabels = nlabels + 1
            end if
        end do

        count_labels = nlabels
    end function count_labels

    integer function count_formats(nlines, lines)
        integer,          intent(in) :: nlines
        character(len=*), intent(in) :: lines(nlines)
        integer                      :: nlabels = 0, i

        do i = 1, nlines
            if ((index(lines(i), 'FORMAT') .ne. 0) .and. (lines(i)(1:6) .ne. '')) then
                nlabels = nlabels + 1
            end if
        end do

        count_formats = nlabels
    end function count_formats

    subroutine collect_lines(lines)
        character(len=1024), intent(inout) :: lines(:)
        character(len=1024)                :: line
        integer                            :: uin = 0, stat, iline = 0

        rewind (uin)

        if (stat < 0) then
            print *, 'File open failed'
            return
        end if

        do
            read (uin, '(A1024)', iostat=stat) line

            if (stat < 0) then
                exit
            else if (is_comment(line) .or. line == '') then
                cycle
            else if (is_cont(line) .and. len(line) >= 7) then
                lines(iline) = trim(lines(iline)) // trim(line(7:))
            else
                iline = iline + 1
                lines(iline) = line
            end if
        end do

        close (uin)
    end subroutine collect_lines

    subroutine collect_statements(nlines, lines, nstatements, statements)
        integer,          intent(in)    :: nlines
        character(len=*), intent(in)    :: lines(nlines)
        integer,          intent(in)    :: nstatements
        integer,          intent(inout) :: statements(nstatements)
        integer                         :: iline, istatement = 1, sno, stat

        do iline = 1, nlines
            if (lines(iline)(1:6) .ne. '') then
                read (lines(iline)(1:6), '(i6)', iostat=stat) sno

                if (stat > 0) then
                    print *, 'Invalid statmement number for line'
                    print *, trim(lines(iline))
                    call exit(1)
                end if

                statements(istatement) = sno
                istatement = istatement + 1
            end if
        end do
    end subroutine collect_statements

    subroutine print_lines(nlines, lines)
        integer,          intent(in)    :: nlines
        character(len=*), intent(inout) :: lines(:)
        integer                         :: i

        do i = 1, nlines
            print *, trim(lines(i))
        end do
    end subroutine print_lines

    subroutine number_statements(nstatements, statements, line_of_statement, nlines, lines)
        integer,          intent(in)    :: nstatements
        integer,          intent(in)    :: statements(nstatements)
        integer,          intent(inout) :: line_of_statement(nstatements)
        integer,          intent(in)    :: nlines
        character(len=*), intent(inout) :: lines(nlines)
        integer                         :: i, sno, index

        do i = 1, nlines
            if (lines(i)(1:6) .ne. '') then
                read (lines(i)(1:6), '(i6)') sno
                index = find(nstatements, statements, sno)
                line_of_statement(index) = i
            end if
        end do
    end subroutine number_statements

    logical function is_arith(line)
        ! Lazy heuristic for kind of parsing arithmetic statements. Not
        ! really complete or correct, just gives some amount of error
        ! message.
        character(len=*), intent(in) :: line
        integer                      :: i, j, k

        i = scan(line, '=')

        if (i < 1) then
            is_arith = .false.
            return
        end if

        j = scan(line, '(')

        if (j < 1 .or. j > i) then
            is_arith = .true.
            return
        end if

        k = match_paren(line(j:)) + j - 1
        is_arith = (k > j .and. k < i)
    end function is_arith

    integer function stmt_type(line)
        character(len=*), intent(in) :: line
        character                    :: one_char
        integer                      :: i, target, stat, result

        i = 7

        do
            if (line (i:i) == ' ') then
                i = i + 1
            else
                exit
            end if
        end do

        if ((line(i:i + 2)) == 'DO ') then
            result = stmt_do
        else if (line(i:i + 4) == 'STOP ') then
            result = stmt_stop
        else if (line(i:i + 2) == 'IF ') then
            result = stmt_if
        else if (line(i:i + 4) == 'READ ') then
            result = stmt_read
        else if (line(i:i + 5) == 'PRINT ') then
            result = stmt_print
        else if (line(i:i + 8) == 'CONTINUE ') then
            result = stmt_continue
        else if (line(i:i + 9) == 'DIMENSION ') then
            result = stmt_dim
        else if (line(i:i + 6) == 'ASSIGN ') then
            result = stmt_assign
        else if (line(i:i + 5) == 'FORMAT') then
            result = stmt_format
        else if (line(i:i + 5) == 'GO TO') then
            i = i + 6

            do
                if (line(i:i) .ne. ' ') then
                    exit
                end if

                i = i + i
            end do

            one_char= line(i:i)
            read (one_char, '(i1)', iostat=stat) target

            if (line(i:i) == '(') then
                result = stmt_comp_goto
            else if (stat == 0) then
                result = stmt_goto
            else
                result = stmt_asgn_goto
            end if
        else if (is_arith(line)) then
            result = stmt_arith
        else
            print *, 'Syntax error: Unknown statement type parsing line'
            print *, trim(line)
            call exit(1)
        end if

        stmt_type = result
    end function stmt_type

    subroutine parse_do(line, target, id, min, max, inc)
        character(len=*), intent(in)  :: line
        character(len=*), intent(out) :: id, min, max, inc
        integer,          intent(out) :: target
        integer                       :: i, j
        character(len=len(line))      :: buf, tbuf

        i = index(line, 'DO')
        buf = line(i + 2:)
        call trimw(buf, buf)

        if (scan(buf, '0123456789') .ne. 1) then
            print *, 'Syntax Error: Missing target in DO'
            print *, trim(line)
            call exit(1)
        end if

        j = index(buf, ' ')

        if (j < 1) then
            print *, 'Syntax Error: Expected space after target in DO'
            print *, trim(line)
            call exit(1)
        end if

        read (buf(:j - 1), '(i6)') target

        buf = buf(j:)
        i = scan(buf, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ')
        j = index(buf, '=')

        if (i < 1 .or. j < j) then
            print *, 'Syntax Error: Bad initializer in DO'
            print *, trim(line)
            call exit(1)
        end if

        id(:) = buf(i:j - 1)

        if (id == '') then
            print *, 'Syntax Error: Missing ident in DO'
            print *, trim(line)
            call exit(1)
        end if

        buf = buf(j + 1:)
        j = index(buf, ',')

        if (j < 1) then
            print *, 'Syntax Error: Missing limit in DO'
            print *, trim(line)
            call exit(1)
        end if

        min = buf(:j - 1)
        buf = buf(j + 1:)
        call trimw(buf, tbuf)
        j = index(tbuf, ',')

        if (j ==  0) then
            max = tbuf
            inc = '1'
        else
            max = tbuf(:j - 1)
            inc= tbuf(j + 1:)
        end if
    end subroutine parse_do

    subroutine parse_assign(line, dst, src_int)
        character(len=*), intent(in)  :: line
        character(len=1024)           :: buf, tbuf, src
        character(len=*), intent(out) :: dst
        integer                       :: i, src_int, stat

        i = index(line, 'ASSIGN')
        buf = line(i + 6:)

        call trimw(buf, tbuf)

        buf = tbuf
        i = index(buf, 'TO')
        src = buf(:i - 1)

        call trimw(src, src)

        read (src, '(i6)', iostat=stat) src_int

        if (stat > 0) then
            print *, 'Syntax Error: Malformed stmt. no. in ASSIGN'
            print *, trim(src)
            call exit(1)
        end if

        dst = buf(i + 2:)

        call trimw(dst, dst)

        if (dst == '') then
            print *, 'Syntax Error: Missing ident in ASSIGN'
            print *, trim(line)
            call exit(1)
        end if
    end subroutine parse_assign

    integer function parse_goto(line)
        character(len=*), intent(in) :: line
        character(len=len(line))     :: buf
        integer                      :: i, target, stat

        i = index(line, 'GO TO')

        call trimw(line(i + 5:), buf)

        read (buf, '(i6)', iostat=stat) target

        if (stat > 0) then
            print *, 'Syntax error: Malformed target in goto'
            print *, trim(line)
            call exit(1)
        end if

        parse_goto = target
    end function parse_goto

    subroutine count_dos(nlines, lines, do_table, nstatements, statements)
        integer,          intent(in)    :: nlines
        character(len=*), intent(inout) :: lines(nlines)
        integer,          intent(in)    :: nstatements
        integer,          intent(inout) :: do_table(nstatements)
        integer,          intent(inout) :: statements(nstatements)
        character(len=MAX_ID)           :: id, min, max, inc
        integer                         :: i, target, index

        do i = 1, nstatements
            do_table(i) = 0
        end do

        do i = 1, nlines
            if (stmt_type(lines(i)) .eq. stmt_do) then
                call parse_do(lines(i), target, id, min, max, inc)

                index = find(nstatements, statements, target)

                if (index == 0) then
                    print *, 'Invalid target in DO:', target
                    call exit(1)
                end if

                do_table(index) = do_table(index) + 1
            end if
        end do
    end subroutine count_dos

    subroutine start_read (line)
        character(len=*), intent(inout) :: line
        integer                         :: i

        line = line(5:)
        i = scan(line, '0123456789')

        line = line(i:)
        i = scan(line, ' ')

        call trimw(line(i:), line)
    end subroutine start_read

    subroutine iter_read (line, next, is_big)
        character(len=*), intent(inout) :: line
        character(len=*), intent(out)   :: next
        logical,          intent(out)   :: is_big
        integer                         :: i, j

        i = index(line, ',')
        j = index(line, '(')

        if (j > 0 .and. (i < 1 .or. j < i)) then
            is_big = .true.
            line = line(j:)
            i = 1
            j = match_paren(line)
            next = line(:j)
            line = line(j + 1:)
            i = index(line, ',')
            line = line(i + 1:)
        else
            is_big = .false.
            i = scan(line, 'ABCDEFGHIJKLMNOPQRTSUVWXYZ')
            j = scan(line, ',')

            if (j == 0) then
                j = len(line) + 1
            end if

            next = line(i:j - 1)
            line = line(j + 1:)
        end if
    end subroutine iter_read

    logical function done_read (line)
        character(len=*), intent(in) :: line

        done_read = (trim(line) == '')
    end function done_read

    integer function collect_vars(nlines, lines, vars)
        integer,          intent(in)    :: nlines
        character(len=*), intent(inout) :: lines(nlines)
        character(len=*), intent(inout) :: vars(:)
        character(len=1024)             :: buf, tbuf
        character(len=MAX_ID)           :: inc, tnext, tdst, tid
        character(len=MAX_ID)           :: next, dst, id, min, max
        integer                         :: src, type, i, j, k, nvars, target
        logical                         :: is_big

        nvars = 0

        do i = 1, nlines
            buf  = lines(i)(7:)
            type = stmt_type(lines(i))

            if (type == stmt_arith) then
                call trimw(buf, tbuf)
                buf = tbuf
                buf = buf(:scan(buf, ' =') - 1)

                if (scan(buf, '(') == 0) then
                    nvars = nvars + insert(nvars, vars, buf)
                end if
            else if (type == stmt_read) then
                call start_read (buf)

                do
                    if (done_read (buf)) then
                        exit
                    end if

                    call iter_read (buf, next, is_big)

                    if (.not. is_big) then
                        call trimw(next, tnext)
                        nvars = nvars + insert(nvars, vars, tnext)
                    else
                        k = scan(next, '=', .true.)
                        j = scan(next(:k - 1), ',', .true.)
                        nvars = nvars + insert(nvars, vars, next(j + 1:k - 1))
                    end if
                end do
            else if (type == stmt_assign) then
                call parse_assign(buf, dst, src)
                call trimw(dst, tdst)
                ! HACK: All labels are integers
                nvars = nvars + insert(nvars, vars, 'N' // tdst)
            else if (type == stmt_do) then
                call parse_do(buf, target, id, min, max, inc)
                call trimw(id, tid)
                nvars = nvars + insert(nvars, vars, tid)
            end if
        end do

        collect_vars = nvars
    end function collect_vars

    integer function collect_dims(nlines, lines, dims)
        integer,          intent(in)  :: nlines
        character(len=*), intent(in)  :: lines(:)
        character(len=*), intent(out) :: dims(:)
        character(len=MAX_DIM)        :: buf
        integer                       :: i, ndims

        ndims = 0

        do i = 1, nlines
            call trimw(lines(i)(7:), buf)

            if (buf(1:9) == 'DIMENSION') then
                call trimw(buf(10:), buf)
                call nospace(buf)
                ndims = ndims + 1
                dims(ndims) = buf
            end if
        end do

        collect_dims = ndims
    end function collect_dims

    logical function is_int(id)
        character(len=*), intent(in) :: id
        integer                      :: i

        i = scan(id, 'IJKLMN')
        is_int = (i == 1)
    end function is_int

    logical function is_prim(str)
        character(len=*), intent(in) :: str

        is_prim = (str == 'ABSF') .or. &
            (str == 'XABSF') .or. &
            (str == 'INTF') .or. &
            (str == 'XINTF') .or. &
            (str == 'MODF') .or. &
            (str == 'XMODF') .or. &
            (str == 'MAX0F') .or. &
            (str == 'MAX1F') .or. &
            (str == 'XMAX0F') .or. &
            (str == 'XMAX1F') .or. &
            (str == 'MIN0F') .or. &
            (str == 'MIN1F') .or. &
            (str == 'XMIN0F') .or. &
            (str == 'XMIN1F')
    end function is_prim

    recursive subroutine parse_exp(in, out)
        character(len=*), intent(inout) :: in
        character(len=*), intent(out)   :: out
        character(len=len(in))          :: rin
        character(len=len(out))         :: rout
        character(len=1)                :: op, start, done
        character(len=2)                :: sep
        integer                         :: i, j

        call trimw(in, in)

        if (in .ne. '') then
            if (in(1:1) == '(') then
                i = match_paren(in)

                if (i == 0) then
                    print *, 'Unmatched paren while parsing expression'
                    print *, trim(in)
                    call exit(1)
                end if

                rin = in(2:i - 1)
                in = in(i + 1:)

                call parse_exp(rin, rout)

                out = '(' // trim(rout) // ')'
            else if (in(1:1) == '-') then
                in = in(2:)

                call parse_exp(in, rout)

                out = '-' // '(' // trim(rout) // ')'
            else if (scan(in(1:1), '0123456789') == 1) then
                i = 2

                do
                    if (scan(in(i:i), '0123456789') .ne. 1) then
                        exit
                    end if

                    i = i + 1
                end do

                if (in(i:i) == '.') then
                    j = i + 1

                    do
                        if (scan(in(j:j), '0123456789') .ne. 1) then
                            exit
                        end if

                        j = j + 1
                    end do

                    out = in(:j - 1)
                    in = in(j:)
                else
                    out = in(:i - 1)
                    in = in(i:)
                end if
            else if (scan(in(1:1), 'ABCDEFGHIJKLMNOPQRSTUVWXYZ') == 1)then
                do j = 2, len(in)
                    if (scan(in(j:j), 'ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_') .ne. 1) then
                        exit
                    end if
                end do

                if (in(j:j) .ne. '(') then
                    out = in(:j - 1)
                    in = in(j:)
                else
                    out = in(:j - 1)

                    if (is_prim(out)) then
                        start = '('
                        sep = ','
                        done = ')'
                    else
                        start = '['
                        sep = ']['
                        done = ']'
                    end if

                    in = in(j:)
                    j = match_paren(in)

                    if (j == 0) then
                        print *, 'Unmatched paren in expr near ', trim(out)
                        call exit(1)
                    end if

                    rin = in(2:)
                    out = trim(out) // start

                    do
                        call trimw(rin, rin)
                        call parse_exp(rin, rout)
                        call trimw(rin, rin)

                        out = trim(out) // rout

                        if (rin(1:1) == ')') then
                            in = rin(2:)
                            exit
                        else
                            out = trim(out) // sep
                            rin = rin(2:)
                        end if
                    end do

                    out = trim(out) // done
                end if
            end if

            call trimw(in, in)

            if (in .ne. '' .and. scan(in, '+-/*') .eq. 1) then
                if (in(1:2) == '**') then
                    in = in(3:)

                    call parse_exp(in, rout)

                    out = 'pow((float)(' // trim(out) //  '),(float)(' // trim(rout)   // '))'
                else
                    op = in(1:1)
                    in = in(2:)

                    call parse_exp(in, rout)

                    out = trim(out) // op // trim(rout)
                end if
            end if
        end if
    end subroutine parse_exp

    integer function match_char(str, c)
        character(len=*) :: str
        character(len=1) :: c
        integer          :: depth, i

        depth = 0
        match_char = 0

        do i = 1, len(str)
            if (str(i:i) == '(') then
                depth = depth + 1
            else if (str(i:i) == ')') then
                depth = depth - 1
            else if (str(i:i) == c .and. depth == 0) then
                match_char = i
                exit
            end if
        end do
    end function match_char

    recursive subroutine gen_io(is_input, fmt, line)
        logical                   :: is_input
        character(len=*)          :: fmt, line
        character(len=MAX_ID)     :: min, max, inc, id
        character(len=MAX_ID * 4) :: bounds
        character(len=1024)       :: exp_buf, in_buf
        integer                   :: uout = 1, j, k, n
        do
            call trimw(line, line)

            if (line == '') then
                exit
            end if

            j = match_char(line, ',')

            if (j == 0) then
                j= len(line) + 1
            end if

            in_buf = line(:j - 1)

            if (line(1:1) == '(') then
                j = match_paren(line)

                k = scan(line(1:j), '=', .true.)
                n = scan(line(1:k), ',', .true.)

                bounds = line(k + 1:j - 1)
                in_buf = line(2:n - 1)
                id = line(n + 1:k - 1)

                j = index(bounds, ',')
                min = bounds(1:j - 1)
                bounds = bounds(j + 1:)

                if (index(bounds, ',') < 1) then
                    max = bounds
                    inc = '1'
                else
                    j = index(bounds, ',')
                    max = bounds(2:j - 1)
                    bounds = bounds(j + 1:)
                    inc=bounds
                end if

                if (min == '') then
                    print *, 'Missing loop min in I/O statement'
                    print *, trim(line)
                    call exit(1)
                else if (max == '') then
                    print *, 'Missing loop max in I/O statement'
                    print *, trim(line)
                    call exit(1)
                else if (id == '') then
                    print *, 'Missing loop variable in I/O statement'
                    print *, trim(line)
                    call exit(1)
                end if

                if (inc == '') inc = '1'

                write (uout, '(a)') 'for(' // trim(id) // '=' // trim(min) // ';' //  trim(id) // '<=' // &
                    trim(max)  // '; ' // trim(id) // ' += ' // trim(inc) // '){'

                in_buf = line(2:n - 1)
                call gen_io(is_input, fmt, in_buf)
                write (uout, '(a)') '}'
                exit
            else
                exp_buf = ''
                call parse_exp(in_buf, exp_buf)
                line = line(j + 1:)

                if (is_input) then
                    write (uout, '(a)') 'in(&f' // trim(fmt) // ',(void**)&' // trim(exp_buf) // ');'
                else
                    write (uout, '(a)') 'out(&f' // trim(fmt) // ',(float)(' // trim(exp_buf) // '));'
                end if
            end if
        end do
    end subroutine gen_io

    subroutine code_gen(output, nvars, variables, ndims, dims, do_table, nstatements, statements, nlines, lines)
        character(len=*), intent(in) :: output
        integer,          intent(in) :: nvars
        character(len=*), intent(in) :: variables(:)
        integer,          intent(in) :: ndims
        character(len=*), intent(in) :: dims(:)
        integer,          intent(in) :: do_table(:)
        integer,          intent(in) :: nstatements
        integer,          intent(in) :: statements(:)
        integer,          intent(in) :: nlines
        character(len=*), intent(in) :: lines(:)
        integer                      :: uout = 1, stat, i, type, stmt, num_braces, target, j, k, rep_cnt, src, width
        character(len=100)           :: braces, spec
        character(len=6)             :: tbuf, less, equal, greater, width_str
        character(len=MAX_ID)        :: id, min, max, inc, dst
        character(len=MAX_DIM)       :: dim_str
        character(len=len(lines(1))) :: line
        character(len=1024)          :: exp_buf, in_buf, fmt_buf
        character(len=20)            :: back_str

        open (uout, file=trim(output) // '.c')

        write (uout, '(a)') '#include <math.h>'
        write (uout, '(a)') '#include <stdio.h>'
        write (uout, '(a)') '#include <stdlib.h>'
        write (uout, '(a)') '#include "runtime.h"'
        write (uout, '(a)') ''
        write (uout, '(a)') 'int main(){'
        write (uout, '(a)') 'int iftmp;'
        write (uout, '(a)') 'format fmt_tmp;'

        do i = 1, nvars
            if (is_int(variables(i))) then
                write (uout, '(3a)') 'int ', trim(variables(i)), ';'
            else
                write (uout, '(3a)') 'float ', trim(variables(i)), ';'
            end if
        end do

        do i = 1, ndims
            j = index(dims(i), '(')
            k = index(dims(i), ')')
            id = dims(i)(:j - 1)
            dim_str = dims(i)(j + 1:k - 1)

            call replace_char(dim_str, ',', '+1][')

            if (is_int(id)) then
                write (uout, '(5a)') 'int ', trim(id), '[', trim(dim_str), '+1];'
            else
                write (uout, '(5a)') 'float ', trim(id), '[', trim(dim_str), '+1];'
            end if
        end do

        do i = 1, nlines
            type = stmt_type(lines(i))
            line = lines(i)

            if (type == stmt_format) then
                tbuf = lines(i)(:6)
                back_str = ', NULL'

                call trimw(tbuf, tbuf)

                j = index(line, '(')

                if (j < 1) then
                    print *, 'Missing open paren in FORMAT'
                    print *, trim(line)
                    call exit(1)
                end if

                line = line(j:)
                j = match_paren(line)

                if (j == 0) then
                    print *, 'Unmatched paren in FORMAT'
                    print *, trim(line)
                    call exit(1)
                end if

                line = lines(i)(index(lines(i), '(') + 1:)
                fmt_buf = 'char*front' // trim(tbuf) // '[] = {'

                do
                    call trimw(line, line)

                    if (line(1:1) == ' ' .or. line(1:1) == ')') then
                        fmt_buf = trim(fmt_buf) //  'NULL}; format f' // trim(tbuf) // '= {front' // trim(tbuf) // &
                            trim(back_str) // '};'
                        exit
                    else if (line(1:1) == '(') then
                        fmt_buf = trim(fmt_buf) // 'NULL}; char* back' // trim(tbuf) // '[] = {'
                        back_str= ',back' // trim(tbuf)
                        line = line(2:)
                    end if

                    spec = line(:scan(line, '/)') - 1)
                    rep_cnt = 1

                    if (scan(spec(1:1), '0123456789') == 1) then
                        j = scan(spec, 'HIEF')

                        read (spec(:j - 1), '(i6)') rep_cnt

                        spec = spec(j:)
                    end if

                    if (spec(1:1) == 'H') then
                        spec = spec(2:)
                        fmt_buf = trim(fmt_buf) // '"' // spec(:rep_cnt) // '",'
                        line = line(rep_cnt + j + 1:)
                    else if (spec(1:1) == 'I') then
                        spec = spec(2:)

                        read (spec(:j - 1), '(i6)') width
                        write (width_str, '(i6)') width + 1

                        call trimw(width_str, width_str)

                        do j = 1, rep_cnt
                            fmt_buf = trim(fmt_buf) // '"%' // trim(width_str) // 'd", '
                        end do

                        line = line(scan(line, ',/)') + 1:)
                    else if (spec(1:1) == 'F') then
                        spec = spec(2:)
                        j = scan(spec, '.')

                        read (spec(:j - 1), '(i6)') width
                        write (width_str, '(i6)') width + 1

                        call trimw(width_str, width_str)

                        spec = spec(j + 1:)

                        do j = 1, rep_cnt
                            fmt_buf = trim(fmt_buf) // '"%' // trim(width_str) // '.' // trim(spec) // 'f", '
                        end do

                        line = line(scan(line, '/)') + 1:)
                    else
                        spec = spec(2:)

                        do j = 1, rep_cnt
                            fmt_buf = trim(fmt_buf) // '"%' // trim(spec) // 'lf", '
                        end do

                        line = line(scan(line, '/)') + 1:)
                    end if
                end do

                write (uout, '(a)') trim(fmt_buf)
            end if
        end do

        do i = 1, nlines
            read (lines(i)(1:6), '(i6)') stmt

            stmt = find(nstatements, statements, stmt)

            if (stmt > 0) then
                call trimw(lines(i)(1:6), tbuf)
                write (uout, '(3a)') 'L', trim(tbuf), ':(void*)0;'
            end if

            type = stmt_type(lines(i))

            if (type == stmt_do) then
                call parse_do(lines(i), target, id, min, max, inc)
                write (uout, '(13a)') 'for(', trim(id), '=', trim(min), ';', trim(id), '<=', trim(max), ';', &
                    trim(id), '+=', trim(inc), '){'
            else if (type == stmt_stop) then
                write (uout, '(a)') 'exit(0);'
            else if (type == stmt_assign) then
                call parse_assign(lines(i), dst, src)

                if (find(nstatements, statements, src) < 1) then
                    print *, 'Invalid statement no. in ASSIGN'
                    print *, trim(lines(i))
                    call exit(1)
                end if

                write (uout, '(2a, i6, a)') 'N' // trim(dst), ' = ', src, ';'
            else if (type == stmt_goto) then
                j = parse_goto(lines(i))

                if (find(nstatements, statements, j) < 1) then
                    print *, 'Invalid target in GOTO'
                    print *, trim(lines(i))
                    call exit(1)
                end if

                write (tbuf, '(i6)') j

                call trimw(tbuf, tbuf)

                write (uout, '(3a)') 'goto L', trim(tbuf), ';'
            else if (type == stmt_asgn_goto) then
                line = lines(i)(index(lines(i), 'goto') + 4:)

                call trimw(line, line)

                j = scan(line, ' (')
                id = line(index(line, 'GO TO') + 5:index(line, '(') - 1)

                call trimw(id, id)

                ! See hack in collect_var
                id = 'N' // id
                line = line(index(line, '(') + 1:)

                write (uout, '(3a)') 'switch(', trim(id), '){'

                do
                    call trimw(line, line)

                    j = scan(line, ' ,)')

                    if (index(line, ',') < 1 ) then
                        write (uout, '(5a)') 'case ', trim(line(:j - 1)), ': goto L', trim(line(:j - 1)), ';'
                        exit
                    else
                        write (uout, '(5a)') 'case ', trim(line(:j - 1)), ': goto L', trim(line(:j - 1)), ';'
                        line = line(index(line, ',') + 1:)
                    end if
                end do

                write (uout, '(a)') '}'
            else if (type == stmt_comp_goto) then
                line = lines(i)(index(lines(i), 'goto') + 4:)

                call trimw(line, line)

                j = scan(trim(line), ' ', .true.)
                id = line(j + 1:)
                line = line(index(line, '(') + 1:)
                k = 1

                write (uout, '(3a)') 'switch(', trim(id), '){'

                do
                    write (tbuf, '(i6)') k

                    call trimw(line, line)

                    j = scan(line, ' ,)')

                    call trimw(tbuf, tbuf)

                    if (index(line, ',') < 1 ) then
                        write (uout, '(5a)') 'case ', trim(tbuf), ': goto L', trim(line(:j - 1)), ';'
                        exit
                    else
                        write (uout, '(5a)') 'case ', trim(tbuf), ': goto L', trim(line(:j - 1)), ';'
                        line = line(index(line, ',') + 1:)
                    end if

                    k = k + 1
                end do

                write (uout, '(a)') '}'
            else if (type == stmt_arith) then
                line = lines(i)(7:)
                id = line(:index(line, '=') - 1)

                call trimw(id, id)

                exp_buf = ''

                call parse_exp(line(index(line, '=') + 1:), exp_buf)
                call replace_char(id, ',', '][')
                call replace_char(id, '(','[')
                call replace_char(id, ')',']')

                write (uout, '(4a)') trim(id), ' = ', trim(exp_buf), ';'
            else if (type == stmt_if) then
                line = lines(i)(index(lines(i), 'IF') + 2:)

                call trimw(line, line)

                j = match_paren(line)

                if (j == 0) then
                    print *, 'Unmatched paren in IF'
                    print *, trim(line)

                    call exit(1)
                end if

                in_buf = line(:j)

                call parse_exp(in_buf, exp_buf)

                line = line(j + 1:)

                j = index(line, ',')

                less = line(:j - 1)
                line = line(j + 1:)

                j = index(line, ',')

                equal = line(:j - 1)
                greater = line(j + 1:)

                call trimw(less, less)
                call trimw(equal, equal)
                call trimw(greater, greater)

                read (less, '(i6)', iostat=stat) j

                if (stat > 0) then
                    print *, 'Malformed statement number in IF'
                    print *, trim(lines(i))
                    call exit(1)
                end if

                if (find(nstatements, statements, j) < 1) then
                    print *, 'Invalid statment number in IF'
                    print *, trim(lines(i))
                    call exit(1)
                end if

                read (equal, '(i6)', iostat=stat) j

                if (stat > 0) then
                    print *, 'Malformed statment number in IF'
                    print *, trim(lines(i))
                    call exit(1)
                end if

                if (find(nstatements, statements, j) < 1) then
                    print *, 'Invalid statment number in IF'
                    print *, trim(lines(i))
                    call exit(1)
                end if

                read (greater, '(i6)', iostat=stat) j

                if (stat > 0) then
                    print *, 'Malformed statment number in IF'
                    print *, trim(lines(i))
                    call exit(1)
                end if

                if (find(nstatements, statements, j) < 1) then
                    print *, 'Invalid statment number in IF'
                    print *, trim(lines(i))
                    call exit(1)
                end if

                write (uout, '(3a)') '{iftmp =' // trim(exp_buf) // ';'
                write (uout, '(3a)') 'if (iftmp < 0) goto L' // trim(less) // ';'
                write (uout, '(3a)') 'if (iftmp == 0) goto L' // trim(equal) // ';'
                write (uout, '(3a)') 'if (iftmp > 0) goto L' // trim(greater) // ';'
                write (uout, '(a)')  '}'
            else if (type == stmt_print) then
                line = lines(i)(index(lines(i), 'PRINT') + 5:)

                call trimw(line, line)

                j = scan(line, '1234567890')
                k = scan(line(j:), ' ')

                tbuf = line(:k - 1)

                if (line(k:) == ' ') then
                    write (uout, '(a)') 'fmt_tmp = f' // trim(line) // ';'
                    write (uout, '(a)') 'nullary_out(&f' // trim(line) // ');'
                    write (uout, '(a)') 'f' // trim(line) // ' = fmt_tmp;'
                    write (uout, '(a)') 'printf("\n");'
                else
                    line = line(k:)

                    write (uout, '(a)') 'fmt_tmp = f' // trim(tbuf) // ';'

                    call gen_io(.false., tbuf, line)

                    write (uout, '(a)') 'f' // trim(tbuf) // ' = fmt_tmp;'
                    write (uout, '(a)') 'printf("\n");'
                end if
            else if (type == stmt_read) then
                line = lines(i)(index(lines(i), 'READ') + 4:)

                call trimw(line, line)

                j = scan(line, '0123456789')
                line = line(j:)

                j = scan(line, ' ')
                tbuf = line(:j - 1)

                call trimw(line(j:), line)

                write (uout, '(a)') 'fmt_tmp = f' // trim(tbuf) // ';'

                call gen_io(.true., tbuf, line)

                write (uout, '(a)') 'f' // trim(tbuf) // ' = fmt_tmp;'
            else if (type == stmt_continue) then
                write (uout, '(a)') 'continue;'
            end if

            if (lines(i)(1:6) == '') cycle

            if (stmt > 0 .and. do_table(stmt) > 0) then
                braces = ''
                num_braces = do_table(stmt)

                call rep_char('}', num_braces, braces)

                write (uout, '(a)') trim(braces)
            end if
        end do

        write (uout, '(a)') 'return 0;'
        write (uout, '(a)') '}'

        close (uout)
    end subroutine code_gen

    subroutine main()
        character(len=1024)              :: input, output
        character(len=1024), allocatable :: lines(:)
        character(len=MAX_DIM)           :: dims(MAX_VARS)
        character(len=MAX_ID)            :: vars(MAX_VARS)
        integer                          :: nstatements, nvars, ndims
        integer                          :: nlines, nformats, nlabels
        integer,             allocatable :: line_of_statement(:)
        integer,             allocatable :: statements(:), do_table(:)

        call get_command_argument(1, input)
        call get_command_argument(2, output)

        nlines = count_lines(input)

        allocate (lines(nlines))

        call collect_lines(lines)

        nformats    = count_formats(nlines, lines)
        nlabels     = count_labels(nlines, lines)
        ndims       = collect_dims(nlines, lines, dims)
        nstatements = nformats + nlabels

        allocate (statements(nstatements))
        allocate (line_of_statement(nstatements))
        allocate (do_table(nstatements))

        call collect_statements(nlines, lines, nstatements, statements)
        call sort(nstatements, statements)
        call number_statements(nstatements, statements, line_of_statement, nlines, lines)
        call count_dos(nlines, lines, do_table, nstatements, statements)

        nvars = collect_vars(nlines, lines, vars)

        ! call print_lines(nlines, lines)
        call code_gen(output, nvars, vars, ndims, dims, do_table, nstatements, statements, nlines, lines)
        call execute_command_line('cc ' // trim(output) // '.c runtime.c -o ' // trim(output) // ' -g -lm')
    end subroutine main
end program compiler
