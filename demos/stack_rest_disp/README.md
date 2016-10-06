# Launch Server

> ./stack_rest_disp

# Launch Client

> ./rest_client

# Test with wget

 To get parameters :

> wget http://localhost:8080/parameters -q -O -

 To delete parameter :

> wget --method=delete http://localhost:8080/parameters/host -q -O -