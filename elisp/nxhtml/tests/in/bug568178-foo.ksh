# Test heredoc
echo "Does this look good?"

cat <<HTML  && exit 0
<head>
</head>
HTML
ls
# How is this fontified?
echo "Hello, world!"
