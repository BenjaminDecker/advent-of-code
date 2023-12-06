const r_number = r"(\d+)"

get_numbers_pt1(line) = [parse(Int, match.match) for match in eachmatch(r_number, line)]

get_numbers_pt2(line) = get_numbers_pt1(replace(line, " " => ""))

function mitternachtsformel(a, b, c)
    let root = sqrt(b^2 - 4 * a * c)
        return ((-b + root) / 2 * a, (-b - root) / 2a)
    end
end

function get_number_of_options(time, distance)
    x_1, x_2 = mitternachtsformel(-1, time, -distance)
    if isinteger(x_1)
        x_1 += 1
    end
    x_1 = Int(ceil(x_1))
    if isinteger(x_2)
        x_2 -= 1
    end
    x_2 = Int(floor(x_2))
    num_possible = x_2 - x_1 + 1
    return num_possible
end

function wait_for_it(lines, part2)
    fun = part2 ? get_numbers_pt2 : get_numbers_pt1
    numbers = map(fun, lines)
    times = numbers[1]
    distances = numbers[2]
    return mapreduce(
        index -> get_number_of_options(times[index], distances[index]),
        *,
        eachindex(times)
    )
end

# pt1
@time println("Part 1: $(wait_for_it(readlines("src/06/input.txt"), false))")

# pt2
@time println("Part 2: $(wait_for_it(readlines("src/06/input.txt"), true))")