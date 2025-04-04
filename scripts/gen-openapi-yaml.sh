#!/usr/bin/env bash
# Read the models file and output a YAML OpenAPI spec which can be used to generate types.

set -e

# models file like persistent's config/models file format
declare modelsFile=$1

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

awk -f- <<'AWK' "$modelsFile"
/^[A-Z]/ {
  print "                  " $1 ":"
  print "                    $ref: '#/components/schemas/" $1 "'"
}
AWK

cat <<YAML
components:
  schemas:
YAML

awk -f- <<'AWK' "$modelsFile"
/^[A-Z]/ {
  entity = $1
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
