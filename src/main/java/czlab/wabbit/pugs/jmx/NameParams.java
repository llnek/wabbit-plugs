/**
 * Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
 * The use and distribution terms for this software are covered by the
 * Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 * which can be found in the file epl-v10.html at the root of this distribution.
 * By using this software in any fashion, you are agreeing to be bound by
 * the terms of this license.
 * You must not remove this notice, or any other, from this software.
 */

package czlab.wabbit.pugs;

import java.util.Arrays;

/**
 * @author Kenneth Leung
 */
public class NameParams {

  private String[] _pms;
  private String _name;

  /**
   */
  public NameParams(String name, String[] params) {
    _pms = Arrays.copyOf(params, params.length);
    _name= name;
  }

  /**
   */
  public NameParams(String name) {
    this(name, new String[0]);
  }

  /**
   */
  public String toString() {
    StringBuilder b=new StringBuilder(_name+"/");
    if (_pms != null) for (int i=0; i < _pms.length; ++i) {
       if (i > 0) { b.append("#"); }
       b.append(_pms[i]);
    }
    return _pms.length > 0 ? b.toString() : _name;
  }

  /**
   */
  public int hashCode() {
    int hash= 31 * (31 + _name.hashCode() );
    if (_pms.length > 0) {
      hash += Arrays.hashCode(_pms );
    }
    return hash;
  }

  /**
   */
  public boolean equals(Object obj) {
    if (obj == null ||
        getClass() != obj.getClass() ) {
      return false;
    }
    NameParams other = (NameParams) obj;
    if ( !_name.equals( other._name)) {
      return false;
    }
    return Arrays.equals(_pms, other._pms);
  }

}


