package in.dogue.eirenicon.fov;

import scala.Function1;
import scala.Tuple2;

import java.util.ArrayList;
import java.util.List;

public class ShadowCastJava {
    final int r;
    final int sx;
    final int sy;
    final int w;
    final int h;
    final List<Tuple2<Integer, Integer>> lst = new ArrayList<Tuple2<Integer, Integer>>();
    public ShadowCastJava(int r, int sx, int sy, int w, int h) {
        this.r = r;
        this.sx = sx;
        this.sy = sy;
        this.w = w;
        this.h = h;
    }
    private static final int[] dirs = {-1, 1,   1, -1,   -1, -1,   1, 1};
    public static List<Tuple2<Integer, Integer>> cast(int px, int py, int r, int w, int h, Function1<Tuple2<Integer, Integer>, Boolean> isSolid) {
        ShadowCastJava shadow = new ShadowCastJava(r, px, py, w, h);
        for (int k = 0; k < dirs.length; k += 2) {
            int i = dirs[k];
            int j = dirs[k+1];
            shadow.castLight(1, 1.0, 0.0, i, 0, 0, j, isSolid);
            shadow.castLight(1, 1.0, 0.0, 0, i, j, 0, isSolid);
        }
        return shadow.lst;
    }

    private void castLight(int row, double start, double end, int xx, int xy, int yx, int yy, Function1<Tuple2<Integer, Integer>, Boolean> isSolid) {
        double newStart = 0.0;
        if (start < end) {
            return;
        }

        boolean blocked = false;
        for (int d = row; d <= r && !blocked; d += 1) {
            int dy = -d;
            for (int dx = -d; dx <= 0; dx += 1) {
                int cx = sx + dx*xx + dy*xy;
                int cy = sy + dx*yx + dy*yy;
                double ls = (dx - 0.5) / (dy + 0.5);
                double rs = (dx + 0.5) / (dy - 0.5);
                if (!(cx >= 0 && cy >= 0 && cx < w && cy < h) || start < rs) {
                    continue;
                } else if (end > ls) {
                    break;
                }

                if (dx*dx + dy*dy <= r*r) {
                    lst.add(new Tuple2<Integer, Integer>(cx, cy));
                }
                boolean solid = isSolid.apply(new Tuple2<Integer, Integer>(cx, cy));
                if (blocked) {
                    if (solid) {
                        newStart = rs;
                    } else {
                        blocked = false;
                        start = newStart;
                    }
                } else if (solid && d <= r) {
                    blocked = true;
                    castLight(d + 1, start, ls, xx, xy, yx, yy, isSolid);
                    newStart = rs;
                }
            }
        }

    }
}
