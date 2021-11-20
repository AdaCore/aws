
Examples:
https://github.com/readmeio/oas-examples

Clone : $HOME/dev/repositories/git/oas-examples

In those examples we have 2.0, 3.0 & 3.1 OAS for petstore.

============

Server: https://petstore.swagger.io

$ curl -X GET "https://petstore.swagger.io/v2/pet/3" -H 'accept: application/json' -H 'api_key: mykey' | jq

$ curl -X GET "https://petstore.swagger.io/v2/store/inventory" -H 'accept: application/json' -H 'api_key: mykey' | jq

$ curl -X GET "https://petstore.swagger.io/v2/pet/findByStatus?status=sold" -H 'accept: application/json' -H 'api_key: mykey' | jq

$ curl -X GET "https://petstore.swagger.io/v2/pet/findByStatus?status=sold&status=pending" -H 'accept: application/json' -H 'api_key: mykey' | jq

$ curl -X POST "https://petstore.swagger.io/v2/store/order" \
  -H 'accept:application/json' \
  -H 'Content-Type: application/json' \
  -H 'api_key: mykey' \
  -d '{
    "id": 1,
    "petId": 8,
    "quantity": 1,
    "shipDate": "2021-11-20T10:40:06.646Z",
    "status": "placed",
    "complete": true
  }' | jq
