#!/usr/bin/env bash
# Read the models file and output a YAML OpenAPI spec which can be used to generate types.

set -e

show_usage() {
  echo "Usage: $0 [-p] [-h] <models-file>"
  echo ""
  echo "Generate OpenAPI YAML spec from models file"
  echo ""
  echo "Options:"
  echo "  -p    Generate Partial variants of entities"
  echo "  -h    Show this help message"
  echo ""
  echo "Arguments:"
  echo "  models-file    Path to the models file"
}

# Parse command line arguments
declare generatePartials=false
declare modelsFile=""

if [[ $# -eq 0 ]]; then
  show_usage
  exit 1
fi

while [[ $# -gt 0 ]]; do
  case $1 in
    -p)
      generatePartials=true
      shift
      ;;
    -h)
      show_usage
      exit 0
      ;;
    *)
      modelsFile="$1"
      shift
      ;;
  esac
done

if [[ -z "$modelsFile" ]]; then
  echo "Error: models file is required"
  echo ""
  show_usage
  exit 1
fi

declare -r partialEntitySuffix=Partial

cat <<YAML
openapi: 3.0.0
info:
  title: ERPNext DocType Spec
  version: 1.0.0
servers: []
paths:
  /test:
    get:
      operationId: dummyOperation
      responses:
        '200':
          description: Successful Request
          content:
            application/json:
              schema:
                type: object
                properties:
YAML

if [[ "$generatePartials" == true ]]; then
  awk -v partialEntitySuffix="$partialEntitySuffix" -f- <<'AWK' "$modelsFile"
/^[A-Z]/ {
  print "                  " $1 ":"
  print "                    $ref: '#/components/schemas/" $1 "'"
  print "                  " $1 partialEntitySuffix ":"
  print "                    $ref: '#/components/schemas/" $1 partialEntitySuffix"'"
}
AWK
else
  awk -f- <<'AWK' "$modelsFile"
/^[A-Z]/ {
  print "                  " $1 ":"
  print "                    $ref: '#/components/schemas/" $1 "'"
}
AWK
fi

cat <<YAML
components:
  schemas:
YAML

declare awkScript=$(cat <<'AWK'
/^[A-Z]/ {
  entity = $1 entitySuffix
  print "    " entity ":"
  print "      title: " entity
  print "      description: " entity
  print "      properties:"
}
/^ *(--.*)?$/ { entity = "" }
/^ / && entity {
  if ($1 == "Required") {
    print "      required:"
    for (i = 2; i <= NF; i++)
      print "        - " $(i)
  } else {
    i = index($0, "--")
    description = i ? substr($0, i + 2) : ""
    switch ($2) {
      case "Int": type = "integer"; break;
      case "Double": type = "number"; break;
      case "Float": type = "number"; break;
      case "Text": type = "string"; break;
      case "String": type = "string"; break;
      case "Bool": type = "boolean"; break;
      default:
        # print "error: unsupported type: " $2 > "/dev/stderr"; exit 1
        if ($2 !~ /^[[A-Z]/) {
          print "error: type name must start with upper-case letter or '[': " $2 > "/dev/stderr"; exit 1
        } else if ($2 ~ /\[.*\]/) {
          type = "array"
          nestedType = substr($2, 2, length($2) - 2)
        } else {
          type = "object"
          nestedType = $2
        }
        break;
    }
    nullable = ($3 == "Maybe") ? "true" : "false"
    print "        " $1 ":"
    print "          description: " description
    print "          type: " type
    print "          readOnly: false"
    print "          nullable: " nullable
    switch (type) {
      case "array":
        print "          items:"
        print "            $ref: '#/" nestedType "'"
        break
      case "object":
        print "          $ref: '#/" nestedType "'"
        break
    }
  }
}
AWK
)

# Print OpenAPI spec as originally defined:
echo
awk -v entitySuffix='' -e "$awkScript" "$modelsFile"

# Print OpenAPI spec without any required fields. This is useful for
# putDoc/postDoc when you only want to set a limited number of fields and
# for getDocList when you only want to fetch a subset of the fields defined.
if [[ "$generatePartials" == true ]]; then
  echo
  grep -vE '^ +Required .*' "$modelsFile" | awk -v entitySuffix="$partialEntitySuffix" -e "$awkScript"
fi
