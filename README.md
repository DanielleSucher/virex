#Virex: a Vim Regular Expressions playground

Inspired by [Rubular](http://rubular.com/), this is a single-page app that offers a playground to experiment with Vim's regular expressions on the web.
  
**Moving parts**  
  
The web app is written in Erlang, using webmachine. It sends the user-input test string and regex pattern to vim directly and uses vim's `:%s/pattern/replacement/g` substitution under the hood to highlight matches and separate out match groups.

**Building and running locally**  
  
Make sure you have vim installed and then run:

0. `make`

1. `./start.sh`

**NGINX**

Use this config if you want to use NGINX to serve the static content, acting as a reverse proxy to /regex API.

    server {
        listen       80; 
        server_name  www.server_name.com;
        access_log   /var/log/nginx/server_name.com.access.log  main;
        root    /YOUR_PATH_OF_INSTALLATION/virex/priv/www/;
 
        location /regex { 
            proxy_pass      http://127.0.0.1:8191;
        }   
    }
