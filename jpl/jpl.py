#!/usr/bin/env python3

import sys
import requests

def main():
  f = open("jpl.in", "r")
  url = 'https://ssd.jpl.nasa.gov/api/horizons_file.api'
  r = requests.post(url, data={'format':'text'}, files={'input': f})
  f.close()

  f = open("jpl.out", "w")
  f.write(r.text)
  f.close()

if __name__ == "__main__":
  main()

