// Copyright (c) 2011 AlphaSierraPapa for the SharpDevelop Team
// 
// Permission is hereby granted, free of charge, to any person obtaining a copy of this
// software and associated documentation files (the "Software"), to deal in the Software
// without restriction, including without limitation the rights to use, copy, modify, merge,
// publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons
// to whom the Software is furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be included in all copies or
// substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
// INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
// PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE
// FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.

using System;
using System.Text;
using System.Runtime.InteropServices;

namespace ICSharpCode.ILSpy
{
	static class NativeMethods
	{
		public const uint WM_COPYDATA = 0x4a;
		
		[DllImport("user32.dll", CharSet = CharSet.Auto)]
		[return: MarshalAs(UnmanagedType.Bool)]
		internal static extern bool EnumWindows(EnumWindowsProc lpEnumFunc, IntPtr lParam);
		
		[DllImport("user32.dll", CharSet = CharSet.Auto)]
		static extern int GetWindowText(IntPtr hWnd, [Out] StringBuilder title, int size);
		
		public static string GetWindowText(IntPtr hWnd, int maxLength)
		{
			StringBuilder b = new StringBuilder(maxLength + 1);
			if (GetWindowText(hWnd, b, b.Capacity) != 0)
				return b.ToString();
			else
				return string.Empty;
		}
		
		[DllImport("user32.dll", CharSet = CharSet.Auto)]
		internal static extern IntPtr SendMessageTimeout(
			IntPtr hWnd, uint msg, IntPtr wParam, ref CopyDataStruct lParam,
			uint flags, uint timeout, out IntPtr result);
		
		[DllImport("user32.dll", CharSet = CharSet.Auto)]
		[return: MarshalAs(UnmanagedType.Bool)]
		internal static extern bool SetForegroundWindow(IntPtr hWnd);


    [DllImport("user32")]
    internal static extern bool GetMonitorInfo(IntPtr hMonitor, MONITORINFO lpmi);

    /// <summary>
    /// 
    /// </summary>
    [DllImport("User32")]
    internal static extern IntPtr MonitorFromWindow(IntPtr handle, int flags);

    [DllImport("user32.dll", CharSet = CharSet.Auto)]
    internal static extern IntPtr SendMessage(IntPtr hWnd, uint Msg, IntPtr wParam,
                                             IntPtr lParam);

    [DllImport("user32")]
    internal static extern Boolean MoveWindow(
      IntPtr hWnd,
      Int32 x, Int32 y,
      Int32 nWidth, Int32 nHeight, Boolean bRepaint);
	}

  /// <summary>
  /// </summary>
  [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Auto)]
  public class MONITORINFO
  {
    /// <summary>
    /// </summary>            
    public int cbSize = Marshal.SizeOf(typeof(MONITORINFO));

    /// <summary>
    /// </summary>            
    public RECT rcMonitor = new RECT();

    /// <summary>
    /// </summary>            
    public RECT rcWork = new RECT();

    /// <summary>
    /// </summary>            
    public int dwFlags = 0;
  }


  /// <summary> Win32 </summary>
  [StructLayout(LayoutKind.Sequential, Pack = 0)]
  public struct RECT
  {
    /// <summary> Win32 </summary>
    public int left;
    /// <summary> Win32 </summary>
    public int top;
    /// <summary> Win32 </summary>
    public int right;
    /// <summary> Win32 </summary>
    public int bottom;

    /// <summary> Win32 </summary>
    public static readonly RECT Empty = new RECT();

    /// <summary> Win32 </summary>
    public int Width
    {
      get { return Math.Abs(right - left); }  // Abs needed for BIDI OS
    }
    /// <summary> Win32 </summary>
    public int Height
    {
      get { return bottom - top; }
    }

    /// <summary> Win32 </summary>
    public RECT(int left, int top, int right, int bottom)
    {
      this.left = left;
      this.top = top;
      this.right = right;
      this.bottom = bottom;
    }


    /// <summary> Win32 </summary>
    public RECT(RECT rcSrc)
    {
      this.left = rcSrc.left;
      this.top = rcSrc.top;
      this.right = rcSrc.right;
      this.bottom = rcSrc.bottom;
    }

    /// <summary> Win32 </summary>
    public bool IsEmpty
    {
      get
      {
        // BUGBUG : On Bidi OS (hebrew arabic) left > right
        return left >= right || top >= bottom;
      }
    }
    /// <summary> Return a user friendly representation of this struct </summary>
    public override string ToString()
    {
      if (this == RECT.Empty) { return "RECT {Empty}"; }
      return "RECT { left : " + left + " / top : " + top + " / right : " + right + " / bottom : " + bottom + " }";
    }

    /// <summary> Determine if 2 RECT are equal (deep compare) </summary>
    public override bool Equals(object obj)
    {
      if (!(obj is RECT)) { return false; }
      return (this == (RECT)obj);
    }

    /// <summary>Return the HashCode for this struct (not garanteed to be unique)</summary>
    public override int GetHashCode()
    {
      return left.GetHashCode() + top.GetHashCode() + right.GetHashCode() + bottom.GetHashCode();
    }


    /// <summary> Determine if 2 RECT are equal (deep compare)</summary>
    public static bool operator ==(RECT rect1, RECT rect2)
    {
      return (rect1.left == rect2.left && rect1.top == rect2.top && rect1.right == rect2.right && rect1.bottom == rect2.bottom);
    }

    /// <summary> Determine if 2 RECT are different(deep compare)</summary>
    public static bool operator !=(RECT rect1, RECT rect2)
    {
      return !(rect1 == rect2);
    }
  }


  /// <summary>
  /// POINT aka POINTAPI
  /// </summary>
  [StructLayout(LayoutKind.Sequential)]
  public struct POINT
  {
    /// <summary>
    /// x coordinate of point.
    /// </summary>
    public int x;
    /// <summary>
    /// y coordinate of point.
    /// </summary>
    public int y;

    /// <summary>
    /// Construct a point of coordinates (x,y).
    /// </summary>
    public POINT(int x, int y)
    {
      this.x = x;
      this.y = y;
    }
  }

  [StructLayout(LayoutKind.Sequential)]
  public struct MINMAXINFO
  {
    public POINT ptReserved;
    public POINT ptMaxSize;
    public POINT ptMaxPosition;
    public POINT ptMinTrackSize;
    public POINT ptMaxTrackSize;
  };


	[return: MarshalAs(UnmanagedType.Bool)]
	delegate bool EnumWindowsProc(IntPtr hWnd, IntPtr lParam);
	
	[StructLayout(LayoutKind.Sequential)]
	struct CopyDataStruct
	{
		public IntPtr Padding;
		public int Size;
		public IntPtr Buffer;

		public CopyDataStruct(IntPtr padding, int size, IntPtr buffer)
		{
			this.Padding = padding;
			this.Size = size;
			this.Buffer = buffer;
		}
	}
}
