/**
 * Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
 * The use and distribution terms for this software are covered by the
 * Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 * which can be found in the file epl-v10.html at the root of this distribution.
 * By using this software in any fashion, you are agreeing to be bound by
 * the terms of this license.
 * You must not remove this notice, or any other, from this software.
 */

package czlab.test.wabbit;

import czlab.wabbit.server.Container;
import czlab.wabbit.io.IoEvent;
import czlab.wabbit.io.IoService;
import czlab.wabbit.io.IoTrigger;
import czlab.xlib.Muble;

/**
 * @author Kenneth Leung
 */
public class MockIOService implements IoService {

  private Container _c;

  /**
   */
  public MockIOService() {
    _c = new MockContainer();
  }

  @Override
  public boolean isEnabled() {
    return false;
  }

  @Override
  public Muble getx() {
    return null;
  }

  @Override
  public Object id() {
    return null;
  }

  @Override
  public String version() {
    return null;
  }

  @Override
  public void init(Object arg0) {
  }

  @Override
  public Object restart(Object a) {
    return this;
  }

  @Override
  public Object start(Object a) {
    return this;
  }

  @Override
  public void stop() {
  }

  @Override
  public Object parent() {
    return null;
  }

  @Override
  public void setParent(Object arg0) {
  }

  @Override
  public void hold(IoTrigger t, long millis) {
  }

  @Override
  public Container server() {
    return _c;
  }

  @Override
  public Object config() {
    return null;
  }

}
