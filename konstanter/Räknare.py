#
# Räknare.py
#
# - 
# - 
#
# Jonatan Sundqvist
# 23 april 2017

# TODO | - 
#        - 


from os   import path
from http import server





# Server
class FysikServer(server.BaseHTTPRequestHandler):

  def do_HEAD(denna):
    denna.send_response(200)
    denna.send_header('Content-type', 'text/html')
    denna.end_headers()
    
  def do_GET(denna):

    ROOT = './ui'
    ext  = path.splitext(denna.path)[1]

    (mime) = {
      '.html'  : ('text/html'),
      '.css'   : ('text/css'),
      '.js'    : ('text/javascript'),
      '.woff'  : ('application/font-woff'),
      '.woff2' : ('application/font-woff2'),
      '.ttf'   : ('application/x-font-truetype'),
      '.otf'   : ('application/x-font-opentype')
    }.get(ext, None)

    (fn, mime) = (ROOT + '/kalkylator.html', 'text/html') if denna.path == '/' else (ROOT + denna.path, mime)

    if mime == None:
      print('Request for {0} ignored.'.format(denna.path))
    else:      
      denna.send_response(200)
      denna.send_header('Content-type', mime)
      denna.end_headers()
      denna.wfile.write(open(fn, mode='rb').read())


def run():
  server_address = ('', 8000)
  httpd = server.HTTPServer(server_address, FysikServer)
  httpd.serve_forever()


if __name__ == '__main__':
  print('Server')
  run()