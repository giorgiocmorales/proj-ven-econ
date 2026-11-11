-- Place the Word title block beneath the retained first-page letterhead.
-- Other formats keep their native metadata title blocks.
function Pandoc(document)
  if FORMAT ~= "docx" then
    return document
  end

  -- A body spacer protects the logo without changing template page geometry or styles.
  local blocks = pandoc.List({
    pandoc.RawBlock("openxml",
      '<w:p><w:pPr><w:spacing w:after="1200"/></w:pPr><w:r><w:t> </w:t></w:r></w:p>')
  })
  local title_roles = {title = "Title", subtitle = "Subtitle", date = "Date"}
  for _, field in ipairs({"title", "subtitle", "date"}) do
    local value = document.meta[field]
    if value then
      blocks:insert(pandoc.Div(
        {pandoc.Para({pandoc.Str(pandoc.utils.stringify(value))})},
        pandoc.Attr("", {}, {["custom-style"] = title_roles[field]})
      ))
      -- Avoid a second automatic title block above the protected body content.
      document.meta[field] = nil
    end
  end
  blocks:extend(document.blocks)
  document.blocks = blocks
  return document
end
