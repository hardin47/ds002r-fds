function CodeBlock(code)
    if not code.classes:includes("cell-code") then
        return nil
    end
    code.text = code.text:gsub("# fmt: skip\n", "")
    code.text = code.text:gsub("# fmt: skip file\n", "")
    return code
end
