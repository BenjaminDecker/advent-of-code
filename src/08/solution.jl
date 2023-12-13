r_instructions = r"([RL]+)"
r_node = r"(?<id>\w+) = \((?<left>\w+), (?<right>\w+)\)"

struct Node
    id::String
    left::String
    right::String
end

function read_instructions(line::String)::Vector{Symbol}
    map(c -> c == 'L' ? :left : :right,
        collect(match(r_instructions, line).match)
    )
end

function Node(line::String)
    let m = match(r_node, line)
        Node(m["id"], m["left"], m["right"])
    end
end

function do_step(node::Node, nodes::Dict{String,Node}, direction::Symbol)::Node
    return nodes[getproperty(node, direction)]
end

function do_step(node::Set{Node}, nodes::Dict{String,Node}, direction::Symbol)::Set{Node}
    return mapreduce(
        n -> do_step(n, nodes, direction),
        (set, n) -> push!(set, n),
        node;
        init=Set{Node}()
    )
end

is_final_node(node::Node)::Bool = node.id == "ZZZ"

function is_final_node(node::Set{Node})::Bool
    mapreduce(
        n -> n.id[end] == 'Z',
        &,
        node
    )
end

get_initial_node_pt1(nodes::Dict{String,Node})::Node = nodes["AAA"]

function get_initial_node_pt2(nodes::Dict{String,Node})::Set{Node}
    return Set{Node}(filter(
        n -> n.id[end] == 'A',
        collect(values(nodes))
    ))
end

function haunted_wasteland(
    instructions::Vector{Symbol},
    initial_nodes::Set{Node},
    nodes::Dict{String,Node}
)::Int
    step = 0
    node = initial_nodes
    while !is_final_node(node)
        instruction = instructions[step%length(instructions)+1]
        node = do_step(node, nodes, instruction)
        step += 1
    end
    return step
end

function haunted_wasteland(lines::Vector{String}, part2::Bool)::Int
    instructions = read_instructions(lines[1])
    nodes = Dict{String,Node}()
    for node in map(Node, lines[3:end])
        nodes[node.id] = node
    end

    if !part2
        return haunted_wasteland(instructions, Set{Node}([nodes["AAA"]]), nodes)
    end

    iterations_per_node = map(
        n -> haunted_wasteland(instructions, Set{Node}([n]), nodes),
        collect(get_initial_node_pt2(nodes))
    )

    return lcm(iterations_per_node)
end

# pt1
@time println("Part 1: $(haunted_wasteland(readlines("src/08/input.txt"), false))")

# pt2
@time println("Part 2: $(haunted_wasteland(readlines("src/08/input.txt"), true))")
