//#include <iostream>
//#include "conduit.hpp"
//#include "conduit_relay.hpp"
//#include "conduit_blueprint.hpp"

#include <conduit.hpp>
#include <conduit_cpp_to_c.hpp>
// conduit python module capi header
#include "conduit_python.hpp"
// embedded interp
#include "python_interpreter.hpp"

// single python interp instance for our example.
PythonInterpreter *interp = NULL;

extern "C" {

  //----------------------------------------------------------------------------
  // returns our static instance of our python interpreter
  // if not already inited initializes it
  //----------------------------------------------------------------------------

  PythonInterpreter *init_python_interpreter() 
  {
      if( interp == NULL)
      {
          interp = new PythonInterpreter();
          if( !interp->initialize() )
          {
              std::cout << "ERROR: interp->initialize() failed " << std::endl;
             return NULL;
          }
          // setup for conduit python c api
          if(!interp->run_script("import conduit"))
          {
              std::cout << "ERROR: `import conduit` failed" << std::endl;
             return NULL;
          }

          if(import_conduit() < 0)
          {
             std::cout << "failed to import Conduit Python C-API";
             return NULL;
          }

          // Turn this on if you want to see every line
          // the python interpreter executes
          //interp->set_echo(true);
      }
      return interp;
  }

  //----------------------------------------------------------------------------
  // access node passed from fortran to python
  //----------------------------------------------------------------------------

  void conduit_fort_to_py(conduit_node *data) {
    // create python interpreter
    PythonInterpreter *pyintp = init_python_interpreter();

    //pyintp->add_system_path("/usr/local/lib/python3.9/site-packages");

    // get global dict and insert wrapped conduit node
    PyObject *py_mod_dict =  pyintp->global_dict();

    // get cpp ref to passed node
    conduit::Node &n = conduit::cpp_node_ref(data);

    // create py object to wrap the conduit node
    PyObject *py_node = PyConduit_Node_Python_Wrap(&n, 0); // python owns => false

    // my_node is set in here statically, it will be used to access node under python
    pyintp->set_dict_object(py_mod_dict, py_node, "my_node"); 

    // trigger script
    bool err = pyintp->run_script_file("process.py", py_mod_dict);
  }

}
