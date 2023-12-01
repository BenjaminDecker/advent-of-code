firstdigit(line::String) = parse(Int, match(r"\d", line).match)

function pt1(lines::Vector{String})
    return mapreduce(
        line -> firstdigit(line) * 10 + firstdigit(reverse(line)),
        +,
        lines
    )
end

pt1(readlines("src/01/input.txt"))