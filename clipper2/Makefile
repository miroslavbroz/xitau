
opt = 

obj = \
  clipper.engine.o \

test: test.cpp $(obj)
	g++ $(opt) $(obj) -o $@ $<

$(obj): %.o:%.cpp
	g++ $(opt) -o $@ -c $<

clean:
	rm $(obj)

