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

import java.io.File;

import czlab.horde.DbApi;
import czlab.horde.JdbcPool;
import czlab.wabbit.server.Cljshim;
import czlab.wabbit.server.Container;
import czlab.wabbit.server.Service;
import czlab.xlib.Muble;
import czlab.xlib.Schedulable;

/**
 * @author Kenneth Leung
 */
public class MockContainer implements Container {

  private Cljshim _clj;

  public MockContainer() {
    _clj= Cljshim.newrt(Thread.currentThread().getContextClassLoader(), "m1");
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
  public Schedulable core() {
    return null;
  }

  @Override
  public String name() {
    return null;
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
  public void init(Object arg) {}

  @Override
  public Object parent() {
    return null;
  }

  @Override
  public void setParent(Object arg0) {
  }

  @Override
  public boolean hasService(Object serviceId) {
    return false;
  }

  @Override
  public Service service(Object serviceId) {
    return null;
  }

  @Override
  public Object loadTemplate(String tpl, Object ctx) {
    return null;
  }

  @Override
  public boolean isEnabled() {
    return false;
  }

  @Override
  public Cljshim cljrt() {
    return _clj;
  }

  @Override
  public Object podConfig() {
    return null;
  }

  @Override
  public byte[] podKeyBits() {
    return null;
  }

  @Override
  public String podKey() {
    return null;
  }

  @Override
  public File podDir() {
    return null;
  }

  @Override
  public JdbcPool acquireDbPool(Object groupid) {
    return null;
  }

  @Override
  public DbApi acquireDbAPI(Object groupid) {
    return null;
  }

  @Override
  public JdbcPool acquireDbPool() {
    return null;
  }

  @Override
  public DbApi acquireDbAPI() {
    return null;
  }

}
