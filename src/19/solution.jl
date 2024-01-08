const r_workflow_steps =
    r"(?<category>[xmas])(?<operator>[<>])(?<value>\d+):(?<dest_workflow>[a-zA-Z]+)"
const r_workflow_info = r"(?<id>[a-zA-Z]+){.*,(?<dest_workflow>[a-zA-Z]+)}"
const r_part = r"{x=(?<x>\d+),m=(?<m>\d+),a=(?<a>\d+),s=(?<s>\d+)}"

struct WorkflowStep
    category::Symbol
    operator::Char
    value::Int
    dest_workflow::String
end

struct Workflow
    id::String
    steps::Vector{WorkflowStep}
    dest_workflow::String
end

struct Part
    x::Int
    m::Int
    a::Int
    s::Int
end

function Workflow(line::String)
    steps = [
        WorkflowStep(
            Symbol(m[:category][1]),
            m[:operator][1],
            parse(Int, m[:value]),
            m[:dest_workflow]
        )
        for m in eachmatch(r_workflow_steps, line)
    ]
    workflow_info = match(r_workflow_info, line)
    return Workflow(workflow_info[:id], steps, workflow_info[:dest_workflow])
end

function Part(line::String)
    part_info = match(r_part, line)
    return Part(
        parse(Int, part_info[:x]),
        parse(Int, part_info[:m]),
        parse(Int, part_info[:a]),
        parse(Int, part_info[:s])
    )
end

category_sum(part::Part)::Int = part.x + part.m + part.a + part.s

function flow_the_work(part::Part, workflow::Workflow)::String
    for step in workflow.steps
        op = step.operator == '<' ? (<) : (>)
        if op(getfield(part, step.category), step.value)
            return step.dest_workflow
        end
    end
    return workflow.dest_workflow
end

function flow_the_work(part::Part, workflows::Dict{String,Workflow})::Bool
    next_workflow_id = "in"
    while true
        next_workflow_id = flow_the_work(part, workflows[next_workflow_id])
        if next_workflow_id == "A"
            return true
        end
        if next_workflow_id == "R"
            return false
        end
    end
end

function aplenty_pt1(workflows::Dict{String,Workflow}, parts::Set{Part})::Int
    return mapreduce(category_sum, +, filter(part -> flow_the_work(part, workflows), parts))
end

function aplenty_pt2(workflows::Dict{String,Workflow}, parts::Set{Part})::Int
    return 1
end

function aplenty(lines::Vector{String}, part2::Bool)::Int
    workflows = Dict{String,Workflow}()
    seperator_idx = (findfirst(isempty, lines))
    workflow_lines = lines[1:seperator_idx-1]
    part_lines = lines[seperator_idx+1:end]
    filter(!isempty, part_lines)

    for workflow_line in workflow_lines
        workflow = Workflow(workflow_line)
        workflows[workflow.id] = workflow
    end

    parts = Set{Part}()
    for part_line in part_lines
        push!(parts, Part(part_line))
    end

    if part2
        return aplenty_pt2(workflows, parts)
    else
        return aplenty_pt1(workflows, parts)
    end
end

input = readlines("src/19/input.txt")

# pt1
@time println("Part 1: $(aplenty(input, false))")

# pt2
@time println("Part 2: $(aplenty(input, true))")
